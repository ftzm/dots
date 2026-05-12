local k = import 'k8s-libsonnet/main.libsonnet';

{
  // Stamp out a standard self-hosted app: deployment, service, config PVC,
  // and private IngressRoute. Includes namespace if not sharing one.
  // Extend with +: for extra volumes.
  //
  // Usage:
  //   selfhosted.new('radarr', 'linuxserver/radarr:latest', 7878, 'radarr.lan.ftzmlab.xyz')
  //   selfhosted.new('radarr', 'linuxserver/radarr:latest', 7878, 'radarr.lan.ftzmlab.xyz', ns='media')
  //
  new(name, image, port, domain, configSize='1Gi', ns=name):: {
    local labels = { 'app.kubernetes.io/name': name },

    [if ns == name then 'namespace']: k.core.v1.namespace.new(ns),

    configPvc: k.core.v1.persistentVolumeClaim.new(name + '-config')
      + k.core.v1.persistentVolumeClaim.metadata.withNamespace(ns)
      + k.core.v1.persistentVolumeClaim.spec.withAccessModes(['ReadWriteOnce'])
      + k.core.v1.persistentVolumeClaim.spec.resources.withRequests({ storage: configSize })
      + k.core.v1.persistentVolumeClaim.spec.withStorageClassName('nfs'),

    deployment: k.apps.v1.deployment.new(name)
      + k.apps.v1.deployment.metadata.withNamespace(ns)
      + k.apps.v1.deployment.spec.withReplicas(1)
      + k.apps.v1.deployment.spec.selector.withMatchLabels(labels)
      + k.apps.v1.deployment.spec.strategy.withType('Recreate')
      + k.apps.v1.deployment.spec.template.metadata.withLabels(labels)
      + k.apps.v1.deployment.spec.template.spec.withContainers([
        k.core.v1.container.new(name, image)
        + k.core.v1.container.withPorts([
          k.core.v1.containerPort.newNamed(port, 'http'),
        ])
        + k.core.v1.container.withVolumeMounts([
          k.core.v1.volumeMount.new('config', '/config'),
        ]),
      ])
      + k.apps.v1.deployment.spec.template.spec.withVolumes([
        k.core.v1.volume.fromPersistentVolumeClaim('config', name + '-config'),
      ]),

    service: k.core.v1.service.new(name, labels, [
      k.core.v1.servicePort.new(port, port),
    ])
    + k.core.v1.service.metadata.withNamespace(ns),

    ingressRoute: {
      apiVersion: 'traefik.io/v1alpha1',
      kind: 'IngressRoute',
      metadata: {
        name: name,
        namespace: ns,
      },
      spec: {
        entryPoints: ['privateweb', 'privatesecure', 'wgweb', 'wgsecure'],
        routes: [{
          match: 'Host(`' + domain + '`)',
          kind: 'Rule',
          services: [{
            name: name,
            port: port,
          }],
        }],
        tls: {},
      },
    },
  },
}
