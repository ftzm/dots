local backup = import '../../lib/backup.libsonnet';
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
          accessLog: {
            enabled: true,
            format: 'json',  // Easier to parse in Loki
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
              http: {
                redirections: {
                  entryPoint: {
                    to: 'websecure',
                    scheme: 'https',
                  },
                },
              },
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

          // Chart >= 41 takes the file-provider config as an object and
          // YAML-formats it itself; the checksum hashes the rendered form.
          local fileProviderContent = ({
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
                deluge: hostRouter('deluge', 'deluge.lan.ftzmlab.xyz'),
                filestash: hostRouter('filestash', 'filestash.lan.ftzmlab.xyz'),
                webdav: hostRouter('webdav', 'dav.lan.ftzmlab.xyz'),
                nzbget: hostRouter('nzbget', 'nzbget.lan.ftzmlab.xyz'),
              },
              services: {
                jellyfin: hostSvc('8096'),
                deluge: hostSvc('8112'),
                filestash: hostSvc('8334'),
                webdav: hostSvc('8085'),
                nzbget: hostSvc('6789'),
              },
            },
          }),

          deployment: {
            dnsPolicy: 'ClusterFirstWithHostNet',
            podAnnotations: {
              'checksum/file-provider': std.md5(std.manifestYamlDoc(fileProviderContent)),
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
            '--entrypoints.privateweb.http.redirections.entryPoint.to=privatesecure',
            '--entrypoints.privateweb.http.redirections.entryPoint.scheme=https',
            '--entrypoints.privatesecure.address=' + config.tailscaleIP + ':443',
            '--entrypoints.wgweb.address=' + config.wgIP + ':80',
            '--entrypoints.wgweb.http.redirections.entryPoint.to=wgsecure',
            '--entrypoints.wgweb.http.redirections.entryPoint.scheme=https',
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
          configs: {
            params: {
              'reposerver.max.combined.directory.manifests.size': '30000000',
            },
          },
          repoServer: {
            livenessProbe: {
              timeoutSeconds: 5,
              periodSeconds: 30,
              failureThreshold: 5,
            },
            readinessProbe: {
              timeoutSeconds: 5,
              periodSeconds: 15,
            },
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

  // CloudNativePG: PostgreSQL operator
  cnpg: {
    local ns = 'cnpg-system',

    namespace: k.core.v1.namespace.new(ns),

    resources: withNamespace(
      helm.template('cloudnative-pg', '../../charts/cloudnative-pg', {
        namespace: ns,
        values: {},
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
            'grafana.ini': {
              security: {
                allow_embedding: true,
                cookie_samesite: 'lax',
              },
              'auth.anonymous': {
                enabled: true,
                org_role: 'Viewer',
              },
            },
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

  // Homepage: unified dashboard with service links and Prometheus metrics
  homepage: {
    local ns = 'homepage',

    namespace: k.core.v1.namespace.new(ns),

    resources: withNamespace(
      helm.template('homepage', '../../charts/homepage', {
        namespace: ns,
        values: {
          enableRbac: true,
          serviceAccount: { create: true },
          env: [
            { name: 'HOMEPAGE_ALLOWED_HOSTS', value: 'home.lan.ftzmlab.xyz' },
          ],
          envFrom: [
            { secretRef: { name: 'homepage-api-keys' } },
          ],
          config: {
            bookmarks: [],
            kubernetes: { mode: 'cluster' },
            docker: {},
            settingsString: |||
              title: ftzmlab
              theme: dark
              color: slate
              headerStyle: clean
              layout:
                Cluster:
                  style: row
                  columns: 6
                Media:
                  style: row
                  columns: 4
                Apps:
                  style: row
                  columns: 3
                Infrastructure:
                  style: row
                  columns: 3
            |||,
            widgets: [],
            local promWidget(label, query, format='number') = {
              widget: {
                type: 'prometheusmetric',
                url: 'http://kube-prometheus-stack-prometheus.monitoring.svc.cluster.local:9090',
                refreshInterval: 30000,
                metrics: [{
                  label: label,
                  query: query,
                  format: { type: format },
                }],
              },
            },
            services: [
              {
                Cluster: [
                  { CPU: promWidget(
                    'Cluster',
                    '100 - (avg(rate(node_cpu_seconds_total{mode="idle"}[5m])) * 100)',
                    'percent',
                  ) },
                  { Memory: promWidget(
                    'Cluster',
                    '100 * (1 - sum(node_memory_MemAvailable_bytes) / sum(node_memory_MemTotal_bytes))',
                    'percent',
                  ) },
                  { 'NFS Pool': promWidget(
                    'Used',
                    '100 * (1 - node_filesystem_avail_bytes{instance="nas",mountpoint="/pool-1"} / node_filesystem_size_bytes{instance="nas",mountpoint="/pool-1"})',
                    'percent',
                  ) },
                  { 'Unhealthy Pods': promWidget(
                    'Count',
                    'count(kube_pod_status_phase{phase!="Running",phase!="Succeeded"} == 1) or vector(0)',
                  ) },
                  { 'Pod Restarts': promWidget(
                    'Last Hour',
                    'floor(sum(increase(kube_pod_container_status_restarts_total[1h])))',
                  ) },
                  { 'Firing Alerts': promWidget(
                    'Active',
                    'count(ALERTS{alertstate="firing",alertname!="Watchdog"}) or vector(0)',
                  ) },
                ],
              },
              {
                Media: [
                  {
                    Radarr: {
                      href: 'https://radarr.lan.ftzmlab.xyz',
                      icon: 'radarr',
                      ping: 'https://radarr.lan.ftzmlab.xyz',
                      widget: {
                        type: 'radarr',
                        url: 'http://radarr.media.svc.cluster.local:7878',
                        key: '{{HOMEPAGE_VAR_RADARR_KEY}}',
                      },
                    },
                  },
                  {
                    Sonarr: {
                      href: 'https://sonarr.lan.ftzmlab.xyz',
                      icon: 'sonarr',
                      ping: 'https://sonarr.lan.ftzmlab.xyz',
                      widget: {
                        type: 'sonarr',
                        url: 'http://sonarr.media.svc.cluster.local:8989',
                        key: '{{HOMEPAGE_VAR_SONARR_KEY}}',
                      },
                    },
                  },
                  {
                    Lidarr: {
                      href: 'https://lidarr.lan.ftzmlab.xyz',
                      icon: 'lidarr',
                      ping: 'https://lidarr.lan.ftzmlab.xyz',
                      widget: {
                        type: 'lidarr',
                        url: 'http://lidarr.media.svc.cluster.local:8686',
                        key: '{{HOMEPAGE_VAR_LIDARR_KEY}}',
                      },
                    },
                  },
                  {
                    Readarr: {
                      href: 'https://readarr.lan.ftzmlab.xyz',
                      icon: 'readarr',
                      ping: 'https://readarr.lan.ftzmlab.xyz',
                      widget: {
                        type: 'readarr',
                        url: 'http://readarr.media.svc.cluster.local:8787',
                        key: '{{HOMEPAGE_VAR_READARR_KEY}}',
                      },
                    },
                  },
                  {
                    Prowlarr: {
                      href: 'https://prowlarr.lan.ftzmlab.xyz',
                      icon: 'prowlarr',
                      ping: 'https://prowlarr.lan.ftzmlab.xyz',
                      widget: {
                        type: 'prowlarr',
                        url: 'http://prowlarr.media.svc.cluster.local:9696',
                        key: '{{HOMEPAGE_VAR_PROWLARR_KEY}}',
                      },
                    },
                  },
                  {
                    Jellyseerr: {
                      href: 'https://jellyseerr.lan.ftzmlab.xyz',
                      icon: 'jellyseerr',
                      ping: 'https://jellyseerr.lan.ftzmlab.xyz',
                    },
                  },
                  {
                    Jellyfin: {
                      href: 'https://jellyfin.ftzmlab.xyz',
                      icon: 'jellyfin',
                    },
                  },
                ],
              },
              {
                Apps: [
                  {
                    Immich: {
                      href: 'https://img.lan.ftzmlab.xyz',
                      icon: 'immich',
                      ping: 'https://img.lan.ftzmlab.xyz',
                      widget: {
                        type: 'immich',
                        url: 'http://immich-server.immich.svc.cluster.local:2283',
                        key: '{{HOMEPAGE_VAR_IMMICH_KEY}}',
                        version: 2,
                      },
                    },
                  },
                  {
                    Navidrome: {
                      href: 'https://navidrome.lan.ftzmlab.xyz',
                      icon: 'navidrome',
                      ping: 'https://navidrome.lan.ftzmlab.xyz',
                    },
                  },
                  {
                    Audiobookshelf: {
                      href: 'https://audiobookshelf.lan.ftzmlab.xyz',
                      icon: 'audiobookshelf',
                      ping: 'https://audiobookshelf.lan.ftzmlab.xyz',
                    },
                  },
                  {
                    Vaultwarden: {
                      href: 'https://vaultwarden.lan.ftzmlab.xyz',
                      icon: 'vaultwarden',
                      ping: 'https://vaultwarden.lan.ftzmlab.xyz',
                    },
                  },
                  {
                    'The Lounge': {
                      href: 'https://irc.lan.ftzmlab.xyz',
                      icon: 'thelounge',
                      ping: 'https://irc.lan.ftzmlab.xyz',
                    },
                  },
                  {
                    ntfy: {
                      href: 'https://ntfy.lan.ftzmlab.xyz',
                      icon: 'ntfy',
                      ping: 'https://ntfy.lan.ftzmlab.xyz',
                    },
                  },
                ],
              },
              {
                Infrastructure: [
                  {
                    ArgoCD: {
                      href: 'https://argo.lan.ftzmlab.xyz',
                      icon: 'argocd',
                    },
                  },
                  {
                    Grafana: {
                      href: 'https://grafana.lan.ftzmlab.xyz',
                      icon: 'grafana',
                    },
                  },
                  {
                    Traefik: {
                      icon: 'traefik',
                    },
                  },
                ],
              },
            ],
          },
        },
      }),
      ns
    ),

    ingressRoute: {
      apiVersion: 'traefik.io/v1alpha1',
      kind: 'IngressRoute',
      metadata: {
        name: 'homepage',
        namespace: ns,
      },
      spec: {
        entryPoints: ['privateweb', 'privatesecure', 'wgweb', 'wgsecure'],
        routes: [{
          match: "Host(`home.lan.ftzmlab.xyz`)",
          kind: 'Rule',
          services: [{
            name: 'homepage',
            port: 3000,
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

  // Vaultwarden: password vault (static NFS PV for stable backup path)
  vaultwarden: {
    local ns = 'vaultwarden',
    local vwMount = storage.nfsMount('vaultwarden', ns, '/pool-1/vaultwarden', '1Gi'),

    dataPv: vwMount.pv,
    dataPvc: vwMount.pvc,
  } + selfhosted.new('vaultwarden', images.vaultwarden, 80, 'vaultwarden.lan.ftzmlab.xyz') {
    // Remove the default config PVC — we use the static NFS mount instead
    configPvc:: null,
    deployment+: {
      spec+: { template+: { spec+: { containers: [
        super.containers[0] {
          volumeMounts: [
            if v.mountPath == '/config' then v { mountPath: '/data', name: 'data' } else v
            for v in super.volumeMounts
          ],
        }
        + k.core.v1.container.withEnvMixin([
          k.core.v1.envVar.new('DOMAIN', 'https://vaultwarden.lan.ftzmlab.xyz'),
          k.core.v1.envVar.new('ROCKET_PORT', '80'),
          {
            name: 'ADMIN_TOKEN',
            valueFrom: {
              secretKeyRef: {
                name: 'vaultwarden-env',
                key: 'ADMIN_TOKEN',
              },
            },
          },
        ]),
      ] } } },
    } + k.apps.v1.deployment.spec.template.spec.withVolumes([
      k.core.v1.volume.fromPersistentVolumeClaim('data', 'vaultwarden'),
    ]),
  },

  // Immich: photo management with ML search
  immich: {
    local ns = 'immich',
    local libraryMount = storage.nfsMount('immich-library', ns, '/pool-1/cloud/photos', '500Gi'),
    local dbBackupMount = storage.nfsMount('immich-db-backup', ns, '/pool-1/k8s/immich-db-backup', '5Gi'),

    namespace: k.core.v1.namespace.new(ns),

    // Immich upload library (existing photos + new uploads)
    libraryPv: libraryMount.pv,
    libraryPvc: libraryMount.pvc,

    // Static NFS PV/PVC for database backups
    dbBackupPv: dbBackupMount.pv,
    dbBackupPvc: dbBackupMount.pvc,

    // CloudNativePG PostgreSQL cluster with VectorChord
    database: {
      apiVersion: 'postgresql.cnpg.io/v1',
      kind: 'Cluster',
      metadata: {
        name: 'immich-database',
        namespace: ns,
      },
      spec: {
        instances: 1,
        imageName: images.cloudnativeVectorchord,
        storage: {
          size: '5Gi',
          storageClass: 'nfs',
        },
        postgresql: {
          shared_preload_libraries: ['vchord.so'],
        },
        bootstrap: {
          initdb: {
            database: 'immich',
            owner: 'immich',
            postInitApplicationSQL: [
              'CREATE EXTENSION vchord CASCADE;',
              'CREATE EXTENSION earthdistance CASCADE;',
            ],
          },
        },
      },
    },

    // Immich Helm chart
    resources: withNamespace(
      helm.template('immich', '../../charts/immich', {
        namespace: ns,
        values: {
          controllers: {
            main: {
              containers: {
                main: {
                  env: {
                    DB_URL: {
                      valueFrom: {
                        secretKeyRef: {
                          name: 'immich-database-app',
                          key: 'uri',
                        },
                      },
                    },
                  },
                },
              },
            },
          },
          immich: {
            persistence: {
              library: {
                existingClaim: 'immich-library',
              },
            },
          },
          valkey: {
            enabled: true,
            persistence: {
              data: {
                enabled: true,
                type: 'persistentVolumeClaim',
                size: '1Gi',
                storageClass: 'nfs',
                accessMode: 'ReadWriteOnce',
              },
            },
          },
          'machine-learning': {
            enabled: true,
            persistence: {
              cache: {
                enabled: true,
                type: 'persistentVolumeClaim',
                size: '10Gi',
                storageClass: 'nfs',
                accessMode: 'ReadWriteOnce',
              },
            },
          },
        },
      }),
      ns
    ),

    // IngressRoute
    ingressRoute: {
      apiVersion: 'traefik.io/v1alpha1',
      kind: 'IngressRoute',
      metadata: {
        name: 'immich',
        namespace: ns,
      },
      spec: {
        entryPoints: ['privateweb', 'privatesecure', 'wgweb', 'wgsecure'],
        routes: [{
          match: "Host(`img.lan.ftzmlab.xyz`)",
          kind: 'Rule',
          services: [{
            name: 'immich-server',
            port: 2283,
          }],
        }],
        tls: {},
      },
    },

    // Daily pg_dump backup. Runs via the shared helper, which joins the storage
    // group so the uid-26 dump pod can actually write to NFS — the inline version
    // here silently failed with EACCES for months (verified + fixed 2026-07-21).
    dbBackupCronJob: backup.pgDumpCronJob(
      'immich-db-backup', ns, images.cloudnativeVectorchord,
      'immich-database-rw', 'immich', 'immich', 'immich-database-app', 'immich-db-backup'
    ),
  },

  // PinePods: self-hosted podcast ecosystem (Rust backend + Postgres + Valkey).
  // Keeps its own auth (native mobile/desktop apps hit the API), so it is NOT
  // behind forwardAuth — its own login is the leak protection.
  pinepods: {
    local ns = 'pinepods',
    local host = 'pinepods.lan.ftzmlab.xyz',
    local labels = { app: 'pinepods' },
    // Static NFS mounts at known paths so both are covered by the NAS borg job.
    local downloadsMount = storage.nfsMount('pinepods-downloads', ns, '/pool-1/k8s/pinepods/downloads', '10Gi'),
    local dbBackupMount = storage.nfsMount('pinepods-db-backup', ns, '/pool-1/k8s/pinepods-db-backup', '5Gi'),

    namespace: k.core.v1.namespace.new(ns),

    // PostgreSQL via CloudNativePG. PinePods' setup script assumes the `postgres`
    // superuser and runs CREATE DATABASE, so superuser access is enabled and the
    // app points at it (matches upstream compose). The operator generates the
    // superuser password in the `pinepods-database-superuser` secret.
    database: {
      apiVersion: 'postgresql.cnpg.io/v1',
      kind: 'Cluster',
      metadata: { name: 'pinepods-database', namespace: ns },
      spec: {
        instances: 1,
        imageName: images.cnpgPostgres,
        enableSuperuserAccess: true,
        storage: { size: '5Gi', storageClass: 'nfs' },
        bootstrap: { initdb: { database: 'pinepods_database', owner: 'pinepods' } },
      },
    },

    // Valkey cache — ephemeral, no persistence needed.
    valkeyDeployment: k.apps.v1.deployment.new('valkey')
                      + k.apps.v1.deployment.metadata.withNamespace(ns)
                      + k.apps.v1.deployment.spec.selector.withMatchLabels({ app: 'valkey' })
                      + k.apps.v1.deployment.spec.template.metadata.withLabels({ app: 'valkey' })
                      + k.apps.v1.deployment.spec.template.spec.withContainers([
                        k.core.v1.container.new('valkey', images.valkey)
                        + k.core.v1.container.withPorts([k.core.v1.containerPort.new(6379)]),
                      ]),
    valkeyService: k.core.v1.service.new('valkey', { app: 'valkey' }, [k.core.v1.servicePort.new(6379, 6379)])
                   + k.core.v1.service.metadata.withNamespace(ns),

    // Occasional downloads of at-risk / obscure episodes (streaming is the
    // default). These saved files are the only irreplaceable thing here, so the
    // volume is a static NFS path included in borg.
    downloadsPv: downloadsMount.pv,
    downloadsPvc: downloadsMount.pvc,

    // pg_dump target — static NFS path, also borg'd.
    dbBackupPv: dbBackupMount.pv,
    dbBackupPvc: dbBackupMount.pvc,

    // Non-secret env (admin PASSWORD comes from the SopsSecret; DB_PASSWORD from
    // the CNPG-generated superuser secret).
    config: k.core.v1.configMap.new('pinepods-env', {
              SEARCH_API_URL: 'https://search.pinepods.online/api/search',
              PEOPLE_API_URL: 'https://people.pinepods.online',
              HOSTNAME: 'https://' + host,
              DB_TYPE: 'postgresql',
              DB_HOST: 'pinepods-database-rw',
              DB_PORT: '5432',
              DB_USER: 'postgres',
              DB_NAME: 'pinepods_database',
              VALKEY_HOST: 'valkey',
              VALKEY_PORT: '6379',
              DEBUG_MODE: 'false',
              TZ: 'Europe/Copenhagen',
              DEFAULT_LANGUAGE: 'en',
              PUID: '0',
              PGID: '1001',
              USERNAME: 'ftzm',
              FULLNAME: 'ftzm',
              EMAIL: 'm@ftzm.org',
            })
            + k.core.v1.configMap.metadata.withNamespace(ns),

    deployment: k.apps.v1.deployment.new('pinepods')
                + k.apps.v1.deployment.metadata.withNamespace(ns)
                + k.apps.v1.deployment.spec.withReplicas(1)
                + k.apps.v1.deployment.spec.selector.withMatchLabels(labels)
                + k.apps.v1.deployment.spec.strategy.withType('Recreate')
                + k.apps.v1.deployment.spec.template.metadata.withLabels(labels)
                + k.apps.v1.deployment.spec.template.spec.withContainers([
                  k.core.v1.container.new('pinepods', images.pinepods)
                  + k.core.v1.container.withPorts([k.core.v1.containerPort.newNamed(8040, 'http')])
                  + k.core.v1.container.withEnvFrom([{ configMapRef: { name: 'pinepods-env' } }])
                  + k.core.v1.container.withEnv([
                    { name: 'DB_PASSWORD', valueFrom: { secretKeyRef: { name: 'pinepods-database-superuser', key: 'password' } } },
                    { name: 'PASSWORD', valueFrom: { secretKeyRef: { name: 'pinepods-admin', key: 'PASSWORD' } } },
                  ])
                  + k.core.v1.container.withVolumeMounts([
                    k.core.v1.volumeMount.new('downloads', '/opt/pinepods/downloads'),
                  ]),
                ])
                + k.apps.v1.deployment.spec.template.spec.withVolumes([
                  k.core.v1.volume.fromPersistentVolumeClaim('downloads', 'pinepods-downloads'),
                ]),

    service: k.core.v1.service.new('pinepods', labels, [k.core.v1.servicePort.new(8040, 8040)])
             + k.core.v1.service.metadata.withNamespace(ns),

    ingressRoute: {
      apiVersion: 'traefik.io/v1alpha1',
      kind: 'IngressRoute',
      metadata: { name: 'pinepods', namespace: ns },
      spec: {
        entryPoints: ['privateweb', 'privatesecure', 'wgweb', 'wgsecure'],
        routes: [{
          match: 'Host(`' + host + '`)',
          kind: 'Rule',
          services: [{ name: 'pinepods', port: 8040 }],
        }],
        tls: {},
      },
    },

    // Daily pg_dump → static NFS path (borg'd off-box). The DB is re-derivable
    // (subscriptions + positions), but with no ZFS snapshot safety net a cheap
    // dump is the durability floor. Same shared helper as Immich.
    dbBackupCronJob: backup.pgDumpCronJob(
      'pinepods-db-backup', ns, images.cnpgPostgres,
      'pinepods-database-rw', 'postgres', 'pinepods_database', 'pinepods-database-superuser', 'pinepods-db-backup'
    ),
  },

  // Forgejo: self-hosted git forge with Actions enabled.
  // The Actions *runner* is NOT here — it runs in an isolated microVM on nuc
  // (untrusted job code must not share a kernel with the cluster). This is only
  // the trusted instance; the runner dials in over the private ingress.
  forgejo:
    local ns = 'forgejo';
    // Env shared by the app container and the admin-bootstrap init container, so
    // both render an identical app.ini (via the image's environment-to-ini step).
    local secretRef(name, key) = {
      name: name,
      valueFrom: { secretKeyRef: { name: 'forgejo-secrets', key: key } },
    };
    local appEnv = [
      k.core.v1.envVar.new('FORGEJO__server__DOMAIN', 'forgejo.lan.ftzmlab.xyz'),
      k.core.v1.envVar.new('FORGEJO__server__ROOT_URL', 'https://forgejo.lan.ftzmlab.xyz/'),
      k.core.v1.envVar.new('FORGEJO__server__SSH_DOMAIN', 'forgejo.lan.ftzmlab.xyz'),
      k.core.v1.envVar.new('FORGEJO__server__SSH_PORT', '30022'),
      k.core.v1.envVar.new('FORGEJO__server__SSH_LISTEN_PORT', '22'),
      k.core.v1.envVar.new('FORGEJO__database__DB_TYPE', 'sqlite3'),
      k.core.v1.envVar.new('FORGEJO__service__DISABLE_REGISTRATION', 'true'),
      k.core.v1.envVar.new('FORGEJO__security__INSTALL_LOCK', 'true'),
      k.core.v1.envVar.new('FORGEJO__actions__ENABLED', 'true'),
      // Pinned crypto keys (sealed). Without these Forgejo self-mints a transient
      // SECRET_KEY each boot; pinning makes a fresh PVC reproducible.
      secretRef('FORGEJO__security__SECRET_KEY', 'SECRET_KEY'),
      secretRef('FORGEJO__security__INTERNAL_TOKEN', 'INTERNAL_TOKEN'),
      secretRef('FORGEJO__oauth2__JWT_SECRET', 'JWT_SECRET'),
    ];
    // NFS-backed volume for scheduled dumps (shipped off-box by the NAS borg job).
    local backup = storage.nfsMount('forgejo-backup', ns, '/pool-1/k8s/forgejo-backup', '10Gi');
    {
      // Data (git repos + sqlite db + config, all under /data) lives on node-local
      // storage. The pod is pinned to nuc regardless, and SQLite/git over NFS carry
      // real file-locking hazards. Durability comes from proper `forgejo dump`
      // archives (scheduled → NAS/borg), NOT from copying a live sqlite file.
      dataPvc: k.core.v1.persistentVolumeClaim.new('forgejo-data')
               + k.core.v1.persistentVolumeClaim.metadata.withNamespace(ns)
               + k.core.v1.persistentVolumeClaim.spec.withAccessModes(['ReadWriteOnce'])
               + k.core.v1.persistentVolumeClaim.spec.resources.withRequests({ storage: '20Gi' })
               + k.core.v1.persistentVolumeClaim.spec.withStorageClassName('local-path'),

      // Git-over-SSH via NodePort so clone URLs resolve from the LAN.
      // SSH_PORT below must match nodePort so Forgejo advertises the right URL.
      sshService: {
        apiVersion: 'v1',
        kind: 'Service',
        metadata: { name: 'forgejo-ssh', namespace: ns },
        spec: {
          type: 'NodePort',
          selector: { 'app.kubernetes.io/name': 'forgejo' },
          ports: [{ name: 'ssh', port: 22, targetPort: 22, nodePort: 30022 }],
        },
      },

      // Instance crypto keys + admin bootstrap creds (kubeseal, decrypted
      // in-cluster by the sealed-secrets controller). Consumed by env above and
      // the init container below. Regenerate with cluster/scripts/create-sealed-secret.sh.
      sealedSecret: {
        apiVersion: 'bitnami.com/v1alpha1',
        kind: 'SealedSecret',
        metadata: { name: 'forgejo-secrets', namespace: ns },
        spec: {
          encryptedData: {
            SECRET_KEY: 'AgA8EUM3kOABz66x5TOIK0d3coVE88C5XiH9/ZF7268aDwrc5fGXpvfd2sMRivdMEovwHeUKHj9VEHiu9YNxFBkV4kFqFE9K9GZdlIGuMaOPlp7eoZsOt4AKIEq6v+PsTf4qsjP02k5fm79VLfaSxMQtlMhpr82C+EGo0x9H5mlcM7phwm9l+Xp1wHiMIfwkSnAadHxcwXZSNl2FK0Ppi6G+Gbx7UWg4Adi0Lmv5ZGf9K+kx70N3RZYcKRslcjLJD4d7I65hSYJkbFiDOHZfaFReDJ6wgK8e2WuWqIBgllsfMEhFjTnTFrounmoIAJEQMi4Daffp33Cozpx5blL2loDmxc5CJ7hFLjmpW7wMCwEsNUCjuEiGxUPi2/Jc66gE43PqPBAXelZeGOgcnwVqvf2NupgXONNIfce2o6MZY8CCzSbyReUSRvny9FXZZ5syXo03iY18BjpAOl5yQIoFuc+WA7NuzQsV7rEKz8nOZPxKBnkuQw4OYIKwNimc2znvpqaw0jQYeRWanit6AQuEPAMFnnqE8maOTV5m+h/CCTBDFjcC1d47ZhUhj+R4NfcDOzVFGED8Ow/D/pRrXQvQSyl6QmXe5v9xpVN4ZqDLte23fU0a/qqDXV5p7rORQ7ic2MnnOefcqv2PtlqDh8h3PcdlsyDkrwEd9Vr9zQQTYc0fPPrYE0nQczNpmddaJssr96LIJTmVb4+aOy6QvT1RLVYE0CD+uFbuoYRRnBd4/jiGC5VZpVrGJoAc2LTb',
            INTERNAL_TOKEN: 'AgAJCIKnEg0aFqC/CBSUuYKifBt7COgv3BJUsEufqEmkk67rzuPoBPgFVFJYDC1FUDPzkZnKxD8jiLRK/D/JDzj+8bmcaJJHIDQOMY9p429l1n7giqFpfFaDCEUcW9662mILJEeMyyVMMnRy4WS8L6v24y6e5x0tFjDaJ43jf6Tu6pr3J8+jqC/FV5SnWFZV5kPVU5wIUzA6oSwlTeGelVSeJCcUvDPGqYr7MNGKplXFXVqWSB7MgYrvrmlXWqPYSgnE4KF/sGDiMSfQY1ljLkaWtnoXCxrHnPelMOk36fOuYi9jLKNh4lclL1MjD+QhQ/gO7KFQQ+LYebmF+W6PI6+0BkM5LVTzL2YZixClVwSXa/MaxZrMjZJ8Y1yiMauU4n1Dk3/Zq4N4p1wvte77FnsVPqHg66l05Jbkwg7v685LJKbBs1+Kq0N3MzlJ6jYuI3JaWWo0dYcs9gf0RPqyhptYynKHchh1rvxxEpFEjzXkXwisiWpp6KHvzVuWm2iOR2XrwYttS+C3U/gT/1ALAEaq2xUkRjr+EmEHfASmlaR2s0k9N5qyDKGoYF62ktwFSt9YcmjUj92JRDazfmve/QGZLsZqU2qQBxev8mBs+QzCj2Ql/oxnPCYcv2kMgchgcPSE8I7DJlQ46+OMnmolUKYMWMI3eQ65RwmSr3oLyuPG6xXKVnU97ZxvwyTFBVLQPcDhMiumHERRE3kwPiDl+UXYXCEAOm+HBv9bgwoVsyyvJCks4ypaN1W1kjsa9kaZq5fFVlG5debf+T11fqXT7UcUrFQYadzG6XdsM/Shr8U81jsA9KougLQUMly2aq9PYSOzsoB7YGQgvqg=',
            JWT_SECRET: 'AgB5PluOwoXOFxqsBINzFFZIfWD5LU0sXCoTJE7Mfq1Cf05wOTIFikbWbANlYabJY8wTzqXfBnSSRcCwTb5Zx2nt+wNy3TQjz+7LGSfbntT1UihTyicRxna0a1jY9RVs0o3U1b6ZLauyqc1emGB6NUnOQZP1/y1wA2cRUYXNOTH5BSkb8Wt8piNWaPzcQESbkdq0EuPwQpTngc/lBOR6VI8fL8EIyjRAuea3Sy7uJ0FH5NwYNl6msd2QxTI/ziDSwHJQKNsVJ7VWcZBAwmqV/T6Iax7IA2swKrKTN38UzS+L5A8fk2rIEUeF8YZjNj3De82B0PkvTT1vocJ5j/B/6+bVre30Ehjhb/iqDPDPA5LpquA0fbWxzXwIGDvGKYSMEdw3EFTUOc8+3pfjgTCza6aDYFOKBVe8awHH8n3MXd2ut1/83r7kaY1UjjLB0yZJOlbje8qF9ZPBKoTz5J3FU23QsH3W9JHmF/ZnLtuPQexlSS60gMrJ2KvMd+WfUQiT5AprmqBKaFCfE3eIlxUzIHDwyMgx8J7KNklcVjhiFiRUAJUF/gIykiEKrQQluveG+VjVg6LjNbxGh3qr5auG9xY2+0NPqnhUFUVx7FHiYY/z9rKppaMzTMzKKLJRhYplOE8T+wBAizDKgTsX4XRnE1yMkKmhM9rYuuu8e8WwSvV5YAVzBC3Bjz1NR2pJ/0WGs4LY3b7085IT5oXvjoF9dzg49ibF/M8jO+SyF/eZ9288W6MpGAZ++wX9vLnI',
            'admin-username': 'AgCKJOg1l9t8Z23TASf8wca6ZEejCvq9hQ0yGLGKQMtmC57V2HBC0nTjETKK+UfVs5tLNIDGZCcCcgIYmBD9+zMReQC+zyQafPqgTIMy1RMUFnWdwTQJrDfgbXrH4v2a+mQZCBYPcEpm8mD2NDNzgCRRMJVx3v+x2IUQdMNpFg3Au1j7gYOLZf7Ix98PE3qiuWw/l0CSDxtop+j8vhdJUVDHbDSm2QAOUL50cJc02Aqiy9KsBuylVNu5mQ9Qj9Yn4UWIYpfw4EZ8T/KDxn3q92/mdGaBuPVlGvlHRXnsmT6zftu33dgjSTKnVx9H3obXDraOfkr8gPyd0A47Bs8UGHIn2JjCue9wDY/ArsfRbCFYz4/BLTzKgl8WKT0zzzgL2Rmk2nBBh1RZqdZiJf3hfvyM5Y/OCQOyWiz4fA1tRcFKYANiBQgC87hMWTimA9j8+VZFPGp03Z4NQ1phN7eZXK2IOx+raJSo7VNcQzD+uAST7Z4K4Rhb70sQPej3QZKABceBOgcU0YOC45658ajiNYKOLG8aTKlB+3pmyjZum1D5tHsRWVePyKma8Za2uCX2ULJraWNzfunVgIyGNMY20i4sx3pHdyKO/J/F8NIOdocBZiRZrAFtAeYsHWs4YgE3huAr0c11TxiIqlpMk1v6mtq6SbqIEBnZtBNthvU2/tHgk9NWdGDjCP5zs++yFYijGN+n834H',
            'admin-password': 'AgCPVswvWsyKNskel0+V7HeyfTyE4xjIAP0qwp6HV5ZQQP6su/SXmpBHVqWamXPpIJ+1Y42VQVCWZCPv8LMAZynMRUplsNmDIj31Tnjvd4x/v/M6rCnXVME1OBWxayjXubMNrboE6ke/lX8wbMl/S1E4YNAxC5gwubIC3u+V63liWi/60358Bl1Ia3siop91+Gc8WYuxChORCUqXp+Z+eXP7XRg6wgc7MXVNnEGqXVMbzdqvvxMUHbFrZSKalBoGOLwi0HJ2OYyFhaWX/jdw7v233K7fQX1ClDtoo3+fY1+LK7tNPbP0X94ZH8f/CqpNPlqMsNV5Tk1B7lGByesORA/ySR8b5t3rgD4FQS2HgsZX/o5j1D/qds0Ptjsk6hhVqe15wZvpL0uC7COR9gMDo+DGOZ0oqsyg73TUcEXDzORYlNCgfx/VHyVn1K6AdZINFnndHXWBXo2Uhpgl/S1A/p7rIySeGbmEYzQMQRURPZH3n4HEX9fFxk0n9sBJgrbc9kbvs9CJBTDuzoUsdXoagwSA8Ycr2qiloMTJ9DYYXYY4I4JxAqF/DJcFC+qI7NeWm8IWf8mkgDYSF/hnIeOblloOe45tFAjzMmN6Z53+znekSaYl/SWGdCJYQvpvSaPZKcZ6SUwibG9tEM9uOqTl47ZJdayLsUM5u3h4AZU+2OK9ySI1MZGt5j5jTDOdm1UHnHVv+I+3gKPj7BU5Xtk5cz1vCpiltg==',
            'admin-email': 'AgAfFQ0ZGCbwTRvz6MUPEhBFp0kW4AQtyTMKPLOVuT2T2cwAq82X3TCXVfexT4DEvxqCOITueZVfp+Zp234yh4o5ZunzPadlZOJRE3ciEdDkhlgYkSfwWEjRYSJcNg38GE2g+2rqUAzcrNqLWwsEQUhDdKBSt5AOQDwmO/zlQx1PGOEiE4zOJ8BhmoyWgW3e4PcrdonVPFYebN9SqkgkdH+1exnCkD3BkUxPoQUL48yNfHJ/KOtLBZLnoei+a9yvA2lMfcM7M02mcX306cbMSOZvkEFouxRvsl8TAul63bOcwy+Mu9fw3TJpeKFXZIaruOhwua02hmgECSzNsac149/3Q2Bsw1PbZU6bP5i7ymirc6r5nRZ+1C0DNmMLaZtwzrKnkL7F8EP3NRbgOEnIQJT+wk9cAX+WX85MfzK/P5RHfN2rwd44n8xTosOS9AeckfqA+CXEINZIRCwRE6DvdO8HZlkg9nObei9OUbPQrhSIv90LFZdLir4K7o5DzTiiZHs4pC9lKgUDpzVaBOY+LHaHS9rgbsQW5dqLwxcwZvH/Alt5ULhBX/tP+E5WSuWWDPAlZ7mLkWH+SktB76ZHpV6Z2lMAJC1GbDzyIoEeC3P3xnEIV9nPIGzej9ScZhenEsKg3lYbQCL/j4o/nTqa/0a3y7baZG3gK14S+Qei5WV/U7ZFAcCFBsYg2T9z/N+cP0UYJ/jdGppBZmDL',
          },
          template: { metadata: { name: 'forgejo-secrets', namespace: ns } },
        },
      },

      // Daily `forgejo dump` → NFS volume; the NAS borg job ships it off-box.
      // Runs as git (uid 1000, the server user) and writes uncompressed tar so
      // borg can dedup unchanged repos across days. The NFS dir is owned
      // 1000:1000 on the NAS so the git-uid job can write it (no_root_squash).
      backupPv: backup.pv,
      backupPvc: backup.pvc,
      dumpCronJob: {
        apiVersion: 'batch/v1',
        kind: 'CronJob',
        metadata: { name: 'forgejo-dump', namespace: ns },
        spec: {
          schedule: '0 2 * * *',
          concurrencyPolicy: 'Forbid',
          successfulJobsHistoryLimit: 3,
          failedJobsHistoryLimit: 3,
          jobTemplate: { spec: { template: { spec: {
            restartPolicy: 'OnFailure',
            containers: [{
              name: 'dump',
              image: images.forgejo,
              command: ['/bin/bash', '-c'],
              args: [
                |||
                  set -eu
                  TS=$(date +%Y%m%d-%H%M%S)
                  su-exec git forgejo dump --config /data/gitea/conf/app.ini --type tar --tempdir /tmp --file /backup/forgejo-$TS.tar
                  find /backup -name 'forgejo-*.tar' -mtime +7 -delete
                |||,
              ],
              volumeMounts: [
                { name: 'data', mountPath: '/data' },
                { name: 'backup', mountPath: '/backup' },
              ],
            }],
            volumes: [
              { name: 'data', persistentVolumeClaim: { claimName: 'forgejo-data' } },
              { name: 'backup', persistentVolumeClaim: { claimName: 'forgejo-backup' } },
            ],
          } } } },
        },
      },
    } + selfhosted.new('forgejo', images.forgejo, 3000, 'forgejo.lan.ftzmlab.xyz') {
      // Use the node-local data mount instead of the default config PVC.
      configPvc:: null,
      deployment+: {
        spec+: { template+: { spec+: {
          // Reconcile the admin account to the sealed creds on every start
          // (secret-authoritative). Reuses the image's own setup so app.ini +
          // schema exist and are git-owned in both fresh and existing volumes.
          initContainers: [{
            name: 'bootstrap-admin',
            image: images.forgejo,
            env: appEnv + [
              secretRef('ADMIN_USERNAME', 'admin-username'),
              secretRef('ADMIN_PASSWORD', 'admin-password'),
              secretRef('ADMIN_EMAIL', 'admin-email'),
            ],
            command: ['/bin/bash', '-c'],
            args: [
              |||
                set -e
                export GITEA_CUSTOM=/data/gitea
                bash /etc/s6/gitea/setup
                CONF=/data/gitea/conf/app.ini
                su-exec git forgejo migrate --config "$CONF"
                if su-exec git forgejo admin user list --config "$CONF" | awk 'NR>1{print $2}' | grep -qx "$ADMIN_USERNAME"; then
                  su-exec git forgejo admin user change-password --config "$CONF" --username "$ADMIN_USERNAME" --password "$ADMIN_PASSWORD" --must-change-password=false
                else
                  su-exec git forgejo admin user create --config "$CONF" --admin --username "$ADMIN_USERNAME" --password "$ADMIN_PASSWORD" --email "$ADMIN_EMAIL" --must-change-password=false
                fi
              |||,
            ],
            volumeMounts: [{ name: 'data', mountPath: '/data' }],
          }],
          containers: [
            super.containers[0] {
              volumeMounts: [
                if v.mountPath == '/config' then v { mountPath: '/data', name: 'data' } else v
                for v in super.volumeMounts
              ],
            }
            + k.core.v1.container.withPortsMixin([
              k.core.v1.containerPort.newNamed(22, 'ssh'),
            ])
            + k.core.v1.container.withEnvMixin(appEnv),
          ],
        } } },
      } + k.apps.v1.deployment.spec.template.spec.withVolumes([
        k.core.v1.volume.fromPersistentVolumeClaim('data', 'forgejo-data'),
      ]),
    },

}
