local config = import '../../lib/config.libsonnet';
local images = import '../../lib/images.libsonnet';
local selfhosted = import '../../lib/selfhosted.libsonnet';
local storage = import '../../lib/storage.libsonnet';
local helm = (import 'tanka-util/helm.libsonnet').new(std.thisFile);
local k = import 'k8s-libsonnet/main.libsonnet';

// Cluster-scoped kinds that should not have namespace set
local clusterScoped = [
  'ClusterRole',
  'ClusterRoleBinding',
  'StorageClass',
  'Namespace',
  'IngressClass',
  'CustomResourceDefinition',
  'GatewayClass',
];

// Add namespace to all namespaced resources
local withNamespace(resources, ns) = {
  [key]: resources[key] + (
    if std.member(clusterScoped, resources[key].kind)
    then {}
    // Preserve namespace if the chart explicitly set a different one
    else if std.objectHas(resources[key].metadata, 'namespace') && resources[key].metadata.namespace != ns
    then {}
    else { metadata+: { namespace: ns } }
  )
  for key in std.objectFields(resources)
};

{
  nfsProvisioner: {
    namespace: k.core.v1.namespace.new('nfs-provisioner'),

    resources: withNamespace(
      helm.template('nfs-provisioner', '../../charts/nfs-subdir-external-provisioner', {
        namespace: 'nfs-provisioner',
        values: {
          nfs: {
            server: config.nasIP,
            path: '/pool-1/k8s',
          },
          storageClass: {
            name: 'nfs',
            defaultClass: true,
          },
        },
      }),
      'nfs-provisioner'
    ),
  },

  // Test app to verify NFS provisioning
  storageTest: {
    local ns = 'storage-test',

    namespace: k.core.v1.namespace.new(ns),

    pvc: k.core.v1.persistentVolumeClaim.new('test-pvc')
      + k.core.v1.persistentVolumeClaim.metadata.withNamespace(ns)
      + k.core.v1.persistentVolumeClaim.spec.withAccessModes(['ReadWriteMany'])
      + k.core.v1.persistentVolumeClaim.spec.resources.withRequests({ storage: '100Mi' }),

    pod: k.core.v1.pod.new('storage-test')
      + k.core.v1.pod.metadata.withNamespace(ns)
      + k.core.v1.pod.spec.withContainers([
        k.core.v1.container.new('busybox', 'busybox')
        + k.core.v1.container.withCommand(['/bin/sh', '-c', 'echo "Written at $(date)" >> /data/test.txt && cat /data/test.txt && sleep 3600'])
        + k.core.v1.container.withVolumeMounts([
          k.core.v1.volumeMount.new('data', '/data'),
        ]),
      ])
      + k.core.v1.pod.spec.withVolumes([
        k.core.v1.volume.fromPersistentVolumeClaim('data', 'test-pvc'),
      ]),
  },

  // Hello world to test internal ingress
  helloWorld: {
    local ns = 'hello-world',
    local labels = { app: 'hello' },

    namespace: k.core.v1.namespace.new(ns),

    deployment: k.apps.v1.deployment.new('hello')
    + k.apps.v1.deployment.metadata.withNamespace(ns)
    + k.apps.v1.deployment.spec.withReplicas(1)
    + k.apps.v1.deployment.spec.selector.withMatchLabels(labels)
    + k.apps.v1.deployment.spec.template.metadata.withLabels(labels)
    + k.apps.v1.deployment.spec.template.spec.withContainers([
      k.core.v1.container.new('nginx', 'nginx:alpine')
      + k.core.v1.container.withPorts([
        k.core.v1.containerPort.newNamed(80, 'http'),
      ]),
    ]),

    service: k.core.v1.service.new('hello', labels, [
      k.core.v1.servicePort.new(80, 80),
    ])
    + k.core.v1.service.metadata.withNamespace(ns),

    // IngressRoute for private-only access (WireGuard only)
    // To make this public too, add 'web' and 'websecure' to entryPoints
    ingressRoute: {
      apiVersion: 'traefik.io/v1alpha1',
      kind: 'IngressRoute',
      metadata: {
        name: 'hello',
        namespace: ns,
      },
      spec: {
        entryPoints: ['privateweb', 'privatesecure', 'wgweb', 'wgsecure'],
        routes: [
          {
            match: "Host(`hello.lan.ftzmlab.xyz`)",
            kind: 'Rule',
            services: [
              {
                name: 'hello',
                port: 80,
              },
            ],
          },
        ],
        tls: {},
      },
    },
  },

  // Traefik ingress controller with dual entrypoints
  // - Public entrypoints (web, websecure) bind to LAN IP
  // - Private entrypoints (privateweb, privatesecure) bind to Tailscale IP
  // - WireGuard entrypoints (wgweb, wgsecure) bind to WireGuard IP
  traefik: {
    local ns = 'traefik',

    namespace: k.core.v1.namespace.new(ns),

    resources: withNamespace(
      helm.template('traefik', '../../charts/traefik', {
        namespace: ns,
        values: {
          // Enable access logs (collected by Alloy -> Loki)
          logs: {
            access: {
              enabled: true,
              format: 'json',  // Easier to parse in Loki
            },
          },

          // Use host network to bind directly to specific IPs
          hostNetwork: true,

          updateStrategy: {
            type: 'Recreate',
            rollingUpdate: null,
          },

          // Ensure Traefik runs on the node with both IPs (public + WireGuard)
          nodeSelector: {
            'kubernetes.io/hostname': 'nuc',
          },


          // Disable LoadBalancer service - we bind directly via hostNetwork
          service: {
            enabled: false,
          },

          // Entrypoints with unique ports go in the chart's ports section.
          // Entrypoints sharing a port across different IPs (private/WG) must
          // use additionalArguments — k8s container spec forbids duplicate
          // containerPort values even with different hostIPs.
          ports: {
            web: {
              port: 80,
              hostIP: config.publicIP,
              expose: { default: false },
            },
            websecure: {
              port: 443,
              hostIP: config.publicIP,
              expose: { default: false },
            },
            torrent: {
              port: 6881,
              expose: { default: false },
            },
            traefik: {
              expose: { default: false },
            },
            metrics: {
              port: 9091,
              expose: { default: false },
            },
          },

          local fileProviderContent = std.manifestYamlDoc({
            local privateEP = ['privateweb', 'privatesecure', 'wgweb', 'wgsecure'],
            local publicEP = ['web', 'websecure'],
            local hostRouter(name, domain, entryPoints=privateEP) = {
              rule: 'Host(`' + domain + '`)',
              service: name,
              entryPoints: entryPoints,
              tls: {},
            },
            // Traefik uses hostNetwork, so 127.0.0.1 reaches host services
            // regardless of which interface they bind to.
            local hostSvc(port) = {
              loadBalancer: {
                servers: [{ url: 'http://127.0.0.1:' + port }],
              },
            },
            http: {
              routers: {
                jellyfin: hostRouter('jellyfin', 'jellyfin.ftzmlab.xyz', publicEP),
                vaultwarden: hostRouter('vaultwarden', 'vaultwarden.lan.ftzmlab.xyz'),
                deluge: hostRouter('deluge', 'deluge.lan.ftzmlab.xyz'),
                immich: hostRouter('immich', 'img.lan.ftzmlab.xyz'),
                filestash: hostRouter('filestash', 'filestash.lan.ftzmlab.xyz'),
                webdav: hostRouter('webdav', 'dav.lan.ftzmlab.xyz'),
                nzbget: hostRouter('nzbget', 'nzbget.lan.ftzmlab.xyz'),
              },
              services: {
                jellyfin: hostSvc('8096'),
                vaultwarden: hostSvc('8222'),
                deluge: hostSvc('8112'),
                immich: hostSvc('2283'),
                filestash: hostSvc('8334'),
                webdav: hostSvc('8085'),
                nzbget: hostSvc('6789'),
              },
            },
          }),

          deployment: {
            dnsPolicy: 'ClusterFirstWithHostNet',
            podAnnotations: {
              'checksum/file-provider': std.md5(fileProviderContent),
            },
          },

          // File provider for NixOS host services: routes directly to host IP:port,
          // bypassing k8s Service/EndpointSlice (which ArgoCD excludes).
          // watch: false = single directory read at startup (no inotify symlink double-read).
          providers: {
            file: {
              enabled: true,
              watch: false,
              content: fileProviderContent,
            },
          },

          // Private/WG entrypoints via additionalArguments (share port 80/443
          // across different IPs, which k8s containerPort spec can't express).
          additionalArguments: [
            '--entrypoints.privateweb.address=' + config.tailscaleIP + ':80',
            '--entrypoints.privatesecure.address=' + config.tailscaleIP + ':443',
            '--entrypoints.wgweb.address=' + config.wgIP + ':80',
            '--entrypoints.wgsecure.address=' + config.wgIP + ':443',
          ],

          // Single IngressClass for standard Ingress resources
          // Note: Standard Ingress resources will be available on ALL entrypoints.
          // Use IngressRoute CRD with entryPoints field for private-only services.
          ingressClass: {
            enabled: true,
            isDefaultClass: true,
          },
        },
      }),
      ns
    ),

    // PodMonitor for Prometheus to scrape Traefik metrics
    podMonitor: {
      apiVersion: 'monitoring.coreos.com/v1',
      kind: 'PodMonitor',
      metadata: {
        name: 'traefik',
        namespace: ns,
      },
      spec: {
        selector: {
          matchLabels: {
            'app.kubernetes.io/name': 'traefik',
          },
        },
        podMetricsEndpoints: [{
          port: 'metrics',
          path: '/metrics',
        }],
      },
    },

    // Wildcard certificate for *.lan.ftzmlab.xyz (in traefik namespace so Traefik can read it)
    wildcardCert: {
      apiVersion: 'cert-manager.io/v1',
      kind: 'Certificate',
      metadata: {
        name: 'lan-wildcard',
        namespace: ns,
      },
      spec: {
        secretName: 'lan-wildcard-tls',
        issuerRef: {
          name: 'letsencrypt',
          kind: 'ClusterIssuer',
        },
        dnsNames: [
          '*.lan.ftzmlab.xyz',
          'lan.ftzmlab.xyz',
          'jellyfin.ftzmlab.xyz',
        ],
      },
    },

    // Default TLS store so all IngressRoutes use the wildcard cert
    defaultTlsStore: {
      apiVersion: 'traefik.io/v1alpha1',
      kind: 'TLSStore',
      metadata: {
        name: 'default',
        namespace: ns,
      },
      spec: {
        defaultCertificate: {
          secretName: 'lan-wildcard-tls',
        },
      },
    },
  },

  // ArgoCD - GitOps continuous delivery
  argocd: {
    local ns = 'argocd',

    namespace: k.core.v1.namespace.new(ns),

    resources: withNamespace(
      helm.template('argocd', '../../charts/argo-cd', {
        namespace: ns,
        values: {
          // Use existing CRDs if already installed
          crds: {
            install: true,
            keep: true,
          },
          // Only needed for initial install to create redis secret.
          // Keep disabled because it causes problems during syncs.
          redisSecretInit: {
            enabled: false,
          },
        },
      }),
      ns
    ),

    // Application that points ArgoCD at this repo's rendered manifests
    app: {
      apiVersion: 'argoproj.io/v1alpha1',
      kind: 'Application',
      metadata: {
        name: 'lab',
        namespace: ns,
      },
      spec: {
        project: 'default',
        source: {
          repoURL: 'https://github.com/ftzm/dots.git',
          targetRevision: 'HEAD',
          path: 'cluster/manifests/lab',
        },
        destination: {
          server: 'https://kubernetes.default.svc',
          namespace: 'default',
        },
        syncPolicy: {
          automated: {
            prune: true,
            selfHeal: true,  // Auto-sync when cluster state drifts
          },
          syncOptions: [
            'ServerSideApply=true',
          ],
        },
      },
    },

    // Traefik IngressRouteTCP for ArgoCD with TLS passthrough
    ingressRoute: {
      apiVersion: 'traefik.io/v1alpha1',
      kind: 'IngressRouteTCP',
      metadata: {
        name: 'argocd',
        namespace: ns,
      },
      spec: {
        entryPoints: ['privatesecure', 'wgsecure'],
        routes: [{
          match: 'HostSNI(`argo.lan.ftzmlab.xyz`)',
          services: [{
            name: 'argocd-server',
            port: 443,
          }],
        }],
        tls: {
          passthrough: true,
        },
      },
    },
  },

  // Sealed Secrets controller for encrypted secrets in git
  sealedSecrets: {
    local ns = 'sealed-secrets',

    namespace: k.core.v1.namespace.new(ns),

    resources: withNamespace(
      helm.template('sealed-secrets', '../../charts/sealed-secrets', {
        namespace: ns,
        values: {},
      }),
      ns
    ),
  },

  // SOPS Secrets Operator: decrypts SopsSecret CRDs in-cluster using age
  sopsOperator: {
    local ns = 'sops-operator',

    namespace: k.core.v1.namespace.new(ns),

    resources: withNamespace(
      helm.template('sops-secrets-operator', '../../charts/sops-secrets-operator', {
        namespace: ns,
        values: {
          secretsAsFiles: [{
            name: 'sops-age-key',
            mountPath: '/mnt/age/',
            secretName: 'sops-age-key',
          }],
          extraEnv: [{
            name: 'SOPS_AGE_KEY_FILE',
            value: '/mnt/age/key',
          }],
        },
      }),
      ns
    ),
  },

  // cert-manager: TLS certificate management with Let's Encrypt
  certManager: {
    local ns = 'cert-manager',

    namespace: k.core.v1.namespace.new(ns),

    resources: withNamespace(
      helm.template('cert-manager', '../../charts/cert-manager', {
        namespace: ns,
        values: {
          crds: { enabled: true },
        },
      }),
      ns
    ),

    // Cloudflare API token: environments/lab/secrets/cloudflare-api-token.enc.yaml
    // (SOPS-encrypted, copied to manifests/ during render, decrypted in-cluster by sops-secrets-operator)

    // ClusterIssuer for Let's Encrypt using Cloudflare DNS-01
    clusterIssuer: {
      apiVersion: 'cert-manager.io/v1',
      kind: 'ClusterIssuer',
      metadata: {
        name: 'letsencrypt',
      },
      spec: {
        acme: {
          server: 'https://acme-v02.api.letsencrypt.org/directory',
          email: 'm@ftzm.org',
          privateKeySecretRef: {
            name: 'letsencrypt-account-key',
          },
          solvers: [{
            dns01: {
              cloudflare: {
                apiTokenSecretRef: {
                  name: 'cloudflare-api-token',
                  key: 'api-token',
                },
              },
            },
          }],
        },
      },
    },

  },

  // Observability stack: Prometheus, Grafana, Loki, Tempo, Alloy
  monitoring: {
    local ns = 'monitoring',

    namespace: k.core.v1.namespace.new(ns),

    // kube-prometheus-stack: Prometheus + Grafana + Alertmanager
    prometheusStack: withNamespace(
      helm.template('kube-prometheus-stack', '../../charts/kube-prometheus-stack', {
        namespace: ns,
        values: {
          prometheus: {
            prometheusSpec: {
              additionalScrapeConfigs: [
                {
                  job_name: 'host-node-exporter',
                  static_configs: [{
                    targets: ['192.168.1.4:9002', '192.168.1.3:9002'],
                    labels: { job: 'node-exporter' },
                  }],
                  relabel_configs: [{
                    source_labels: ['__address__'],
                    regex: '192.168.1.4:.*',
                    target_label: 'instance',
                    replacement: 'nuc',
                  }, {
                    source_labels: ['__address__'],
                    regex: '192.168.1.3:.*',
                    target_label: 'instance',
                    replacement: 'nas',
                  }],
                },
              ],
              storageSpec: {
                volumeClaimTemplate: {
                  spec: {
                    storageClassName: 'nfs',
                    accessModes: ['ReadWriteOnce'],
                    resources: {
                      requests: { storage: '20Gi' },
                    },
                  },
                },
              },
              retention: '30d',
              retentionSize: '18GB',
              // Discover all ServiceMonitors/PodMonitors, not just those with release label
              podMonitorSelectorNilUsesHelmValues: false,
              serviceMonitorSelectorNilUsesHelmValues: false,
            },
          },
          alertmanager: {
            config: {
              global: {
                resolve_timeout: '5m',
              },
              inhibit_rules: [
                {
                  equal: ['namespace', 'alertname'],
                  source_matchers: ['severity = critical'],
                  target_matchers: ['severity =~ warning|info'],
                },
                {
                  equal: ['namespace', 'alertname'],
                  source_matchers: ['severity = warning'],
                  target_matchers: ['severity = info'],
                },
                {
                  equal: ['namespace'],
                  source_matchers: ['alertname = InfoInhibitor'],
                  target_matchers: ['severity = info'],
                },
                {
                  target_matchers: ['alertname = InfoInhibitor'],
                },
              ],
              receivers: [
                { name: 'null' },
                {
                  name: 'ntfy',
                  webhook_configs: [{
                    url: 'http://ntfy.ntfy.svc.cluster.local/alerts?template=alertmanager',
                    send_resolved: true,
                  }],
                },
              ],
              route: {
                group_by: ['namespace'],
                group_interval: '5m',
                group_wait: '30s',
                receiver: 'ntfy',
                repeat_interval: '12h',
                routes: [
                  {
                    matchers: ['alertname = "Watchdog"'],
                    receiver: 'null',
                  },
                ],
              },
              templates: ['/etc/alertmanager/config/*.tmpl'],
            },
            alertmanagerSpec: {
              storage: {
                volumeClaimTemplate: {
                  spec: {
                    storageClassName: 'nfs',
                    accessModes: ['ReadWriteOnce'],
                    resources: {
                      requests: { storage: '1Gi' },
                    },
                  },
                },
              },
            },
          },
          grafana: {
            admin: {
              existingSecret: 'grafana-admin',
              userKey: 'admin-user',
              passwordKey: 'admin-password',
            },
            persistence: {
              enabled: true,
              storageClassName: 'nfs',
              size: '1Gi',
            },
            additionalDataSources: [
              {
                name: 'Loki',
                type: 'loki',
                url: 'http://loki-gateway.monitoring.svc.cluster.local:80',
                access: 'proxy',
              },
              {
                name: 'Tempo',
                type: 'tempo',
                uid: 'tempo',
                url: 'http://tempo.monitoring.svc.cluster.local:3100',
                access: 'proxy',
              },
            ],
            ingress: { enabled: false },
            sidecar: {
              dashboards: { enabled: true, searchNamespace: 'ALL' },
              datasources: { enabled: true },
            },
          },
          // Disable components not accessible in homelab k8s
          kubeEtcd: { enabled: false },
          kubeControllerManager: { enabled: false },
          kubeScheduler: { enabled: false },
          kubeProxy: { enabled: false },
          // Fix node-exporter mount propagation issue
          'prometheus-node-exporter': {
            hostRootFsMount: {
              enabled: false,
            },
          },
          // Disabled alerts for homelab
          // See values.yaml defaultRules.disabled for mechanism
          defaultRules: {
            disabled: {
              // Cluster is too small to tolerate node failure.
              KubeMemoryOvercommit: true,
              // Tailscale creates a WireGuard interface (wg0) whose kernel module
              // does not populate standard Linux network error/packet counters.
              // node_exporter reads these bogus counters, producing +Inf error
              // ratios (errors / 0 packets = +Inf). Not a real network issue.
              NodeNetworkTransmitErrs: true,
            },
          },
        },
      }),
      ns
    ),

    // Loki: Log aggregation (monolithic mode)
    loki: withNamespace(
      helm.template('loki', '../../charts/loki', {
        namespace: ns,
        values: {
          deploymentMode: 'SingleBinary',
          loki: {
            auth_enabled: false,
            commonConfig: { replication_factor: 1 },
            storage: { type: 'filesystem' },
            schemaConfig: {
              configs: [{
                from: '2024-01-01',
                store: 'tsdb',
                object_store: 'filesystem',
                schema: 'v13',
                index: { prefix: 'index_', period: '24h' },
              }],
            },
            limits_config: {
              retention_period: '720h',
            },
            compactor: {
              retention_enabled: true,
              delete_request_store: 'filesystem',
            },
          },
          singleBinary: {
            replicas: 1,
            persistence: {
              enabled: true,
              storageClass: 'nfs',
              size: '20Gi',
            },
          },
          backend: { replicas: 0 },
          read: { replicas: 0 },
          write: { replicas: 0 },
          gateway: {
            enabled: true,
            replicas: 1,
            service: {
              type: 'NodePort',
              nodePort: 30100,
            },
          },
          minio: { enabled: false },
          test: { enabled: false },
          lokiCanary: { enabled: false },
        },
      }),
      ns
    ),

    // Tempo: Distributed tracing
    tempo: withNamespace(
      helm.template('tempo', '../../charts/tempo', {
        namespace: ns,
        values: {
          tempo: {
            retention: '336h',  // 14 days (Tempo recommended default)
            receivers: {
              otlp: {
                protocols: {
                  grpc: { endpoint: '0.0.0.0:4317' },
                  http: { endpoint: '0.0.0.0:4318' },
                },
              },
            },
          },
          persistence: {
            enabled: true,
            storageClassName: 'nfs',
            size: '10Gi',
          },
        },
      }),
      ns
    ),

    // Alloy: Unified collector for logs and traces
    alloy: withNamespace(
      helm.template('alloy', '../../charts/alloy', {
        namespace: ns,
        values: {
          alloy: {
            mounts: {
              extra: [
                { name: 'journal', mountPath: '/var/log/journal', readOnly: true },
                { name: 'machine-id', mountPath: '/etc/machine-id', readOnly: true },
              ],
            },
            configMap: {
              content: |||
                // Discover all pods
                discovery.kubernetes "pods" {
                  role = "pod"
                }

                // Relabel to extract useful Kubernetes labels
                discovery.relabel "pods" {
                  targets = discovery.kubernetes.pods.targets

                  // Keep only running pods
                  rule {
                    source_labels = ["__meta_kubernetes_pod_phase"]
                    regex         = "Pending|Succeeded|Failed|Completed"
                    action        = "drop"
                  }

                  // Set namespace label
                  rule {
                    source_labels = ["__meta_kubernetes_namespace"]
                    target_label  = "namespace"
                  }

                  // Set pod label
                  rule {
                    source_labels = ["__meta_kubernetes_pod_name"]
                    target_label  = "pod"
                  }

                  // Set container label
                  rule {
                    source_labels = ["__meta_kubernetes_pod_container_name"]
                    target_label  = "container"
                  }

                  // Set node label
                  rule {
                    source_labels = ["__meta_kubernetes_pod_node_name"]
                    target_label  = "node"
                  }

                  // Set app label from pod labels (common conventions)
                  rule {
                    source_labels = ["__meta_kubernetes_pod_label_app"]
                    target_label  = "app"
                  }
                  rule {
                    source_labels = ["__meta_kubernetes_pod_label_app_kubernetes_io_name"]
                    target_label  = "app"
                  }
                }

                // Collect logs from pods
                loki.source.kubernetes "pods" {
                  targets    = discovery.relabel.pods.output
                  forward_to = [loki.process.default.receiver]
                }

                // Process logs: parse JSON and extract labels
                loki.process "default" {
                  forward_to = [loki.write.default.receiver]

                  // Extract JSON fields (silently ignored if not JSON)
                  stage.json {
                    expressions = {
                      level   = "level",
                      msg     = "msg",
                      message = "message",
                    }
                  }

                  // Normalize level to lowercase
                  stage.template {
                    source   = "level"
                    template = "{{ "{{" }} ToLower .Value {{ "}}" }}"
                  }

                  // Only promote level to label if it's a standard value
                  // Selector requires a label match, so we use namespace which is always set
                  stage.match {
                    selector = "{namespace=~\".+\"} |~ \"\\\"level\\\"\\\\s*:\\\\s*\\\"(error|warn|info|debug|ERROR|WARN|INFO|DEBUG)\\\"\""
                    stage.labels {
                      values = { level = "" }
                    }
                  }
                }

                // Collect host journal logs
                loki.source.journal "host" {
                  path          = "/var/log/journal"
                  relabel_rules = discovery.relabel.journal.rules
                  forward_to    = [loki.write.default.receiver]
                  labels        = { job = "systemd-journal" }
                }

                discovery.relabel "journal" {
                  targets = []
                  rule {
                    source_labels = ["__journal__systemd_unit"]
                    target_label  = "unit"
                  }
                  rule {
                    source_labels = ["__journal_priority_keyword"]
                    target_label  = "level"
                  }
                  rule {
                    source_labels = ["__journal__hostname"]
                    target_label  = "host"
                  }
                  rule {
                    source_labels = ["__journal_syslog_identifier"]
                    target_label  = "syslog_identifier"
                  }
                }

                // Write logs to Loki
                loki.write "default" {
                  endpoint {
                    url = "http://loki-gateway.monitoring.svc.cluster.local:80/loki/api/v1/push"
                  }
                }

                // OTLP receiver for traces from instrumented apps
                otelcol.receiver.otlp "default" {
                  grpc { endpoint = "0.0.0.0:4317" }
                  http { endpoint = "0.0.0.0:4318" }
                  output {
                    traces = [otelcol.processor.batch.default.input]
                  }
                }

                // Batch processor for better performance
                otelcol.processor.batch "default" {
                  output {
                    traces = [otelcol.exporter.otlp.tempo.input]
                  }
                }

                // Export traces to Tempo
                otelcol.exporter.otlp "tempo" {
                  client {
                    endpoint = "tempo.monitoring.svc.cluster.local:4317"
                    tls { insecure = true }
                  }
                }
              |||,
            },
          },
          controller: {
            type: 'daemonset',
            volumes: {
              extra: [
                { name: 'journal', hostPath: { path: '/var/log/journal', type: 'Directory' } },
                { name: 'machine-id', hostPath: { path: '/etc/machine-id', type: 'File' } },
              ],
            },
            affinity: {
              nodeAffinity: {
                requiredDuringSchedulingIgnoredDuringExecution: {
                  nodeSelectorTerms: [{
                    matchExpressions: [{
                      key: 'kubernetes.io/hostname',
                      operator: 'NotIn',
                      values: ['friendlywrt'],
                    }],
                  }],
                },
              },
            },
          },
          serviceAccount: { create: true },
          rbac: { create: true },
        },
      }),
      ns
    ),

    // Sealed secret for Grafana admin credentials
    grafanaAdminSecret: {
      apiVersion: 'bitnami.com/v1alpha1',
      kind: 'SealedSecret',
      metadata: {
        name: 'grafana-admin',
        namespace: ns,
      },
      spec: {
        encryptedData: {
          'admin-password': 'AgBLDxlOgkWRdht/nC5yG9qVhvXkL9c8uuQM+noyKPz+Owg2DorZ7OyOCD6NVW7Nw8MZjq0z3yw8VJA/DfCOWWEpj2lA3SDHb2xKHwbdhOcGtok3acVf1lePDi5MDg/hJLEX2m2W/3hxJ6F7OV9EVI1Y+dTeEdKjYkmGewWvO/v5SMtsrPqm+nhbif1WKPPRmzBZkKtHtwMBipunzmR6x1Zf1O9Pne3AQPVzDdBVTQcy0o+/TvKhO85LefO475hX8Uj/j2DzscCGAMbdf5dGhvK+E+ZnuvrfHABN2f+dguOOnZH3zPA8TukWadpM5aN5XQOh5wgRZbNhhDOMI8UHgHLPP7AGm9dp7b7cD4BoDL43OsgL8VognXjfFER/g6e0mkeZMO/ocdTJE/NUjq3LKFUt0k4sok64jnDf9yVPNe0k2etQuRQjC1u6RGvu0HndeVwh7MtXKAQr1fVsaYW5rtGrfFgOusKi/o+qJquRA+/iBf+eF1zAaSB35DpxZl0nVc3lsD6IJsze543nDhvGT7+JRBc6R2ghNIdKoyGpI8IKHK0VH/4tC97NnwcPPH9OdiyraE3ZbzrP9dPmGmM8YSh3BpEGNPY0oo6C7gWJZ8IeqchbIhOz6Cc2UZVw6e/av2FgGfgPYCxhdEnk9kaMgSKzwIeV57TLMPIZ6mL+NiqD4U+knBxK/b2GOzmRzvJTrGR56JpY0HWrcNIKBQIDTXuge6RF1W0UJnp7e6EHpHR2lQ==',
          'admin-user': 'AgBbt9wXLlI8Kjh5YAoHiutyr26U7zuCXc6VpmLu5l3OCM9mKif/TwMb9do6BjPMIoiTsq1jQHsH/N4E16mxR0iht6Awwu7rUgxtlY1zCNQVddXev89AW/yn/+o2W55id14Xig8pOngVA1VNKo9bQCyWKdXNEc2l1IMXEx9Q6uenfGSsdkIrUIY64LkegWvQHSDoj8HTLE8MwCvUcelKNDSmU5QOETjmrpe0oBaWxizbQ+cs3x9uD4b1coS5c3QxjJ32GL89fE4Cx3cOLFa2co44hi+3SUR3yRprGEAbgF9ylWkZ5R3bXvFmDRMKA0gSQnozB0G+RaC/Plvu3/4gYU7A1iKXfnfBzd45ssxrFmzuOZw4tiyftC3rCB8tn2mFYMFHfUvlUYsyWJHjtSaMrt2kBo15kAbkM256L2Oa9qlvQ+V/yCniybunKFB1x2DrBnPCRRWQsnE2/CYvodicIjLogcIKz3yk1vbPoZqoMIgmfa9JifHNQlyOCrtb+Vz4XZf2gOtfildL95YiZWPCsXrMmY9s43xI+dMGeSWOZXICZP8aCakqS+3tobhyyw/OBv4lAPv65QtT72GuzvhlbjLcNqLmG6QdoghW0LHlNKRfb4k3fnV0t38/wKQYiLRyGJfH4lIpWRiyba+ALGPBadJdPIWnbOSgKTSVAB570CiANJl7ZcbyZrlgJUdgU6dxfql5CgZvCg==',
        },
        template: {
          metadata: {
            name: 'grafana-admin',
            namespace: ns,
          },
        },
      },
    },

    // Traefik IngressRoute for Loki (private only, for NAS Promtail)
    lokiIngress: {
      apiVersion: 'traefik.io/v1alpha1',
      kind: 'IngressRoute',
      metadata: {
        name: 'loki',
        namespace: ns,
      },
      spec: {
        entryPoints: ['privateweb', 'privatesecure', 'wgweb', 'wgsecure'],
        routes: [{
          match: "Host(`loki.lan.ftzmlab.xyz`)",
          kind: 'Rule',
          services: [{
            name: 'loki-gateway',
            port: 80,
          }],
        }],
        tls: {},
      },
    },

    // Traefik IngressRoute for Grafana (private/WireGuard only)
    grafanaIngress: {
      apiVersion: 'traefik.io/v1alpha1',
      kind: 'IngressRoute',
      metadata: {
        name: 'grafana',
        namespace: ns,
      },
      spec: {
        entryPoints: ['privateweb', 'privatesecure', 'wgweb', 'wgsecure'],
        routes: [{
          match: "Host(`grafana.lan.ftzmlab.xyz`)",
          kind: 'Rule',
          services: [{
            name: 'kube-prometheus-stack-grafana',
            port: 80,
          }],
        }],
        tls: {},
      },
    },
  },

  // Blocky: Ad-blocking DNS proxy for Tailscale clients
  blocky: {
    local ns = 'blocky',
    local labels = { 'app.kubernetes.io/name': 'blocky' },

    namespace: k.core.v1.namespace.new(ns),

    local configData = {
        'config.yaml': |||
          # Upstream DNS servers
          upstreams:
            groups:
              default:
                - 1.1.1.1
                - 8.8.8.8
                - 9.9.9.9

          bootstrapDns:
            - tcp+udp:1.1.1.1

          # Ad-blocking
          blocking:
            denylists:
              ads:
                - https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts
                - https://adaway.org/hosts.txt
                - https://v.firebog.net/hosts/AdguardDNS.txt
              malware:
                - https://v.firebog.net/hosts/Prigent-Malware.txt
            clientGroupsBlock:
              default:
                - ads
                - malware
            blockType: zeroIp
            refreshPeriod: 24h

          # Custom DNS mappings
          customDNS:
            mapping:
              lan.ftzmlab.xyz: %(tailscaleIP)s

          # Forward cluster.local to CoreDNS
          conditional:
            mapping:
              cluster.local: 10.96.0.10

          caching:
            minTime: 5m
            maxTime: 30m
            prefetching: true

          ports:
            dns:
              - %(publicIP)s:53
              - %(tailscaleIP)s:53
              - %(wgIP)s:53
            http: 4000

          log:
            level: info

          prometheus:
            enable: true
            path: /metrics
        ||| % {
          tailscaleIP: config.tailscaleIP,
          publicIP: config.publicIP,
          wgIP: config.wgIP,
        },
    },

    configmap: k.core.v1.configMap.new('blocky-config')
      + k.core.v1.configMap.metadata.withNamespace(ns)
      + k.core.v1.configMap.withData(configData),

    deployment: k.apps.v1.deployment.new('blocky')
      + k.apps.v1.deployment.metadata.withNamespace(ns)
      + k.apps.v1.deployment.spec.withReplicas(1)
      + k.apps.v1.deployment.spec.selector.withMatchLabels(labels)
      + k.apps.v1.deployment.spec.strategy.withType('Recreate')
      + k.apps.v1.deployment.spec.template.metadata.withLabels(labels)
      + k.apps.v1.deployment.spec.template.metadata.withAnnotations({
        'checksum/config': std.md5(std.toString(configData)),
      })
      + k.apps.v1.deployment.spec.template.spec.withHostNetwork(true)
      + k.apps.v1.deployment.spec.template.spec.withDnsPolicy('ClusterFirstWithHostNet')
      + k.apps.v1.deployment.spec.template.spec.withNodeSelector({
        'kubernetes.io/hostname': 'nuc',
      })
      + k.apps.v1.deployment.spec.template.spec.withContainers([
        k.core.v1.container.new('blocky', images.blocky)
        + k.core.v1.container.withArgs(['--config', '/config/config.yaml'])
        + k.core.v1.container.withPorts([
          k.core.v1.containerPort.new(53) + k.core.v1.containerPort.withName('dns-udp') + k.core.v1.containerPort.withProtocol('UDP'),
          k.core.v1.containerPort.new(53) + k.core.v1.containerPort.withName('dns-tcp') + k.core.v1.containerPort.withProtocol('TCP'),
          k.core.v1.containerPort.newNamed(4000, 'http'),
        ])
        + k.core.v1.container.withVolumeMounts([
          k.core.v1.volumeMount.new('config', '/config'),
        ])
        + k.core.v1.container.readinessProbe.httpGet.withPath('/api/blocking/status')
        + k.core.v1.container.readinessProbe.httpGet.withPort(4000)
        + k.core.v1.container.livenessProbe.httpGet.withPath('/api/blocking/status')
        + k.core.v1.container.livenessProbe.httpGet.withPort(4000),
      ])
      + k.apps.v1.deployment.spec.template.spec.withVolumes([
        k.core.v1.volume.fromConfigMap('config', 'blocky-config'),
      ]),

    podMonitor: {
      apiVersion: 'monitoring.coreos.com/v1',
      kind: 'PodMonitor',
      metadata: {
        name: 'blocky',
        namespace: ns,
      },
      spec: {
        selector: {
          matchLabels: labels,
        },
        podMetricsEndpoints: [{
          port: 'http',
          path: '/metrics',
        }],
      },
    },
  },

  // ntfy: Self-hosted push notification service
  ntfy: {
    local ns = 'ntfy',
    local labels = { 'app.kubernetes.io/name': 'ntfy' },

    namespace: k.core.v1.namespace.new(ns),

    local configData = {
        'server.yml': |||
          base-url: "https://ntfy.lan.ftzmlab.xyz"
          listen-http: ":80"
          cache-file: "/var/cache/ntfy/cache.db"
          cache-duration: "12h"
          auth-default-access: "read-write"
        |||,
    },

    configmap: k.core.v1.configMap.new('ntfy-config')
      + k.core.v1.configMap.metadata.withNamespace(ns)
      + k.core.v1.configMap.withData(configData),

    pvc: k.core.v1.persistentVolumeClaim.new('ntfy-cache')
      + k.core.v1.persistentVolumeClaim.metadata.withNamespace(ns)
      + k.core.v1.persistentVolumeClaim.spec.withAccessModes(['ReadWriteOnce'])
      + k.core.v1.persistentVolumeClaim.spec.resources.withRequests({ storage: '1Gi' })
      + k.core.v1.persistentVolumeClaim.spec.withStorageClassName('nfs'),

    deployment: k.apps.v1.deployment.new('ntfy')
      + k.apps.v1.deployment.metadata.withNamespace(ns)
      + k.apps.v1.deployment.spec.withReplicas(1)
      + k.apps.v1.deployment.spec.selector.withMatchLabels(labels)
      + k.apps.v1.deployment.spec.strategy.withType('Recreate')
      + k.apps.v1.deployment.spec.template.metadata.withLabels(labels)
      + k.apps.v1.deployment.spec.template.metadata.withAnnotations({
        'checksum/config': std.md5(std.toString(configData)),
      })
      + k.apps.v1.deployment.spec.template.spec.withContainers([
        k.core.v1.container.new('ntfy', images.ntfy)
        + k.core.v1.container.withArgs(['serve'])
        + k.core.v1.container.withPorts([
          k.core.v1.containerPort.newNamed(80, 'http'),
        ])
        + k.core.v1.container.withVolumeMounts([
          k.core.v1.volumeMount.new('config', '/etc/ntfy'),
          k.core.v1.volumeMount.new('cache', '/var/cache/ntfy'),
        ]),
      ])
      + k.apps.v1.deployment.spec.template.spec.withVolumes([
        k.core.v1.volume.fromConfigMap('config', 'ntfy-config'),
        k.core.v1.volume.fromPersistentVolumeClaim('cache', 'ntfy-cache'),
      ]),

    service: k.core.v1.service.new('ntfy', labels, [
      k.core.v1.servicePort.new(80, 80),
    ])
    + k.core.v1.service.metadata.withNamespace(ns),

    ingressRoute: {
      apiVersion: 'traefik.io/v1alpha1',
      kind: 'IngressRoute',
      metadata: {
        name: 'ntfy',
        namespace: ns,
      },
      spec: {
        entryPoints: ['privateweb', 'privatesecure', 'wgweb', 'wgsecure'],
        routes: [{
          match: "Host(`ntfy.lan.ftzmlab.xyz`)",
          kind: 'Rule',
          services: [{
            name: 'ntfy',
            port: 80,
          }],
        }],
        tls: {},
      },
    },
  },

  // Media pipeline: arr apps + support services (download clients stay on NixOS)
  media: {
    local ns = 'media',
    local ms = storage.mediastack(ns),
    // PUID/PGID for linuxserver containers — matches NAS storage group
    local storageEnv = [
      k.core.v1.envVar.new('PUID', '0'),
      k.core.v1.envVar.new('PGID', '1001'),
      k.core.v1.envVar.new('TZ', 'Europe/Copenhagen'),
    ],
    local mediaApp(name, image, port, domain) =
      selfhosted.new(name, image, port, domain, ns=ns) {
        deployment+: k.apps.v1.deployment.spec.template.spec.withVolumesMixin([
          k.core.v1.volume.fromPersistentVolumeClaim('media', 'mediastack'),
        ]) + {
          spec+: { template+: { spec+: { containers: [
            super.containers[0]
            + k.core.v1.container.withEnv(storageEnv)
            + k.core.v1.container.withVolumeMountsMixin([
              k.core.v1.volumeMount.new('media', '/data'),
            ]),
          ] } } },
        },
      },

    namespace: k.core.v1.namespace.new(ns),
    mediastackPv: ms.pv,
    mediastackPvc: ms.pvc,

    // Arr apps (all mount mediastack for hardlinks)
    radarr: mediaApp('radarr', images.radarr, 7878, 'radarr.lan.ftzmlab.xyz'),
    sonarr: mediaApp('sonarr', images.sonarr, 8989, 'sonarr.lan.ftzmlab.xyz'),
    lidarr: mediaApp('lidarr', images.lidarr, 8686, 'lidarr.lan.ftzmlab.xyz'),
    readarr: mediaApp('readarr', images.readarr, 8787, 'readarr.lan.ftzmlab.xyz'),

    // Support services (no mediastack volume)
    prowlarr: selfhosted.new('prowlarr', images.prowlarr, 9696, 'prowlarr.lan.ftzmlab.xyz', ns=ns),
    flaresolverr: selfhosted.new('flaresolverr', images.flaresolverr, 8191, 'flaresolverr.lan.ftzmlab.xyz', ns=ns),
    jellyseerr: selfhosted.new('jellyseerr', images.jellyseerr, 5055, 'jellyseerr.lan.ftzmlab.xyz', ns=ns) {
      deployment+: {
        spec+: { template+: { spec+: { containers: [
          super.containers[0] {
            volumeMounts: [v { mountPath: '/app/config' } for v in super.volumeMounts],
          },
        ] } } },
      },
    },
  },

  // Navidrome: music streaming
  navidrome: {
    local ns = 'navidrome',
    local musicMount = storage.nfsMount('music', ns, '/pool-1/mediastack/media/music', '500Gi'),

    musicPv: musicMount.pv,
    musicPvc: musicMount.pvc,
  } + selfhosted.new('navidrome', images.navidrome, 4533, 'navidrome.lan.ftzmlab.xyz') {
    deployment+: k.apps.v1.deployment.spec.template.spec.withVolumesMixin([
      k.core.v1.volume.fromPersistentVolumeClaim('music', 'music'),
    ]) + {
      spec+: { template+: { spec+: { containers: [
        super.containers[0] {
          // Navidrome uses /data not /config
          volumeMounts: [
            if v.mountPath == '/config' then v { mountPath: '/data' } else v
            for v in super.volumeMounts
          ],
        }
        + k.core.v1.container.withVolumeMountsMixin([
          k.core.v1.volumeMount.new('music', '/music') + k.core.v1.volumeMount.withReadOnly(true),
        ])
        + k.core.v1.container.withEnv([
          k.core.v1.envVar.new('ND_MUSICFOLDER', '/music'),
        ]),
      ] } } },
    },
  },

  // Audiobookshelf: audiobook/podcast server
  audiobookshelf: {
    local ns = 'audiobookshelf',
    local abMount = storage.nfsMount('audiobooks', ns, '/pool-1/mediastack/media/audiobooks', '100Gi'),

    audiobooksPv: abMount.pv,
    audiobooksPvc: abMount.pvc,
  } + selfhosted.new('audiobookshelf', images.audiobookshelf, 80, 'audiobookshelf.lan.ftzmlab.xyz') {
    deployment+: k.apps.v1.deployment.spec.template.spec.withVolumesMixin([
      k.core.v1.volume.fromPersistentVolumeClaim('audiobooks', 'audiobooks'),
    ]) + {
      spec+: { template+: { spec+: { containers: [
        super.containers[0]
        + k.core.v1.container.withVolumeMountsMixin([
          k.core.v1.volumeMount.new('audiobooks', '/audiobooks'),
        ]),
      ] } } },
    },
  },

  // The Lounge: IRC client
  thelounge: selfhosted.new('thelounge', images.thelounge, 9000, 'irc.lan.ftzmlab.xyz') {
    deployment+: {
      spec+: { template+: { spec+: { containers: [
        super.containers[0] {
          volumeMounts: [
            if v.mountPath == '/config' then v { mountPath: '/var/opt/thelounge' } else v
            for v in super.volumeMounts
          ],
        },
      ] } } },
    },
  },

}
