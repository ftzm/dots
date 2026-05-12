local config = import 'config.libsonnet';
local k = import 'k8s-libsonnet/main.libsonnet';

{
  nfs: {
    server: config.nasIP,
  },

  // Create a static NFS PV + PVC pair.
  // PV name includes namespace for global uniqueness; PVC name is just `name`.
  // Multiple namespaces can each get their own PV/PVC pointing to the same NFS path.
  nfsMount(name, ns, path, size='10Gi'):: {
    local pvName = name + '-' + ns,

    pv: {
      apiVersion: 'v1',
      kind: 'PersistentVolume',
      metadata: { name: pvName },
      spec: {
        capacity: { storage: size },
        accessModes: ['ReadWriteMany'],
        persistentVolumeReclaimPolicy: 'Retain',
        storageClassName: pvName,
        nfs: {
          server: config.nasIP,
          path: path,
        },
      },
    },

    pvc: k.core.v1.persistentVolumeClaim.new(name)
      + k.core.v1.persistentVolumeClaim.metadata.withNamespace(ns)
      + k.core.v1.persistentVolumeClaim.spec.withAccessModes(['ReadWriteMany'])
      + k.core.v1.persistentVolumeClaim.spec.resources.withRequests({ storage: size })
      + k.core.v1.persistentVolumeClaim.spec.withStorageClassName(pvName)
      + k.core.v1.persistentVolumeClaim.spec.withVolumeName(pvName),
  },

  // Shared mediastack volume — all arr apps mount the same NFS path
  // to enable hardlinks across downloads and media directories.
  //
  // Usage:
  //   local media = storage.mediastack('radarr');
  //   { mediaPv: media.pv, mediaPvc: media.pvc }
  //
  mediastack(ns):: self.nfsMount('mediastack', ns, '/pool-1/k8s/mediastack', '100Gi'),

  // Pipeline contract: which apps access which mediastack paths.
  // Readable top-to-bottom for verification.
  appAccess: {
    deluge: { paths: ['/downloads'], mode: 'rw' },
    nzbget: { paths: ['/downloads'], mode: 'rw' },
    radarr: { paths: ['/downloads', '/media/movies'], mode: 'rw' },
    sonarr: { paths: ['/downloads', '/media/tv'], mode: 'rw' },
    lidarr: { paths: ['/downloads', '/media/music'], mode: 'rw' },
    readarr: { paths: ['/downloads', '/media/books'], mode: 'rw' },
    prowlarr: { paths: [], mode: 'none' },
    jellyfin: { paths: ['/media'], mode: 'ro' },
  },
}
