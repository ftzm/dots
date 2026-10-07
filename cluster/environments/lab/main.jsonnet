// One Tanka inline environment per namespace, so `render-lab` exports one
// directory per ArgoCD Application (ARGOCD_APPLICATIONS_PLAN.md, step 2).
// The resources themselves are defined in lab.jsonnet; this file only says
// which top-level key belongs to which namespace.
local lab = import 'lab.jsonnet';

local namespaceOf = {
  argocd: 'argocd',
  audiobookshelf: 'audiobookshelf',
  blocky: 'blocky',
  certManager: 'cert-manager',
  cnpg: 'cnpg-system',
  externalDns: 'external-dns',
  forgejo: 'forgejo',
  forgejoTofu: 'forgejo',
  healthchecks: 'healthchecks',
  helloWorld: 'hello-world',
  homepage: 'homepage',
  immich: 'immich',
  media: 'media',
  miniflux: 'miniflux',
  monitoring: 'monitoring',
  navidrome: 'navidrome',
  nfsProvisioner: 'nfs-provisioner',
  ntfy: 'ntfy',
  observability: 'monitoring',
  pinepods: 'pinepods',
  sealedSecrets: 'sealed-secrets',
  sopsOperator: 'sops-operator',
  thelounge: 'thelounge',
  traefik: 'traefik',
  vaultwarden: 'vaultwarden',
};

local unmapped = [key for key in std.objectFields(lab) if !std.objectHas(namespaceOf, key)];
local namespaces = std.set(std.objectValues(namespaceOf));

assert unmapped == [] : 'lab.jsonnet keys with no namespace in main.jsonnet: ' + std.join(', ', unmapped);

{
  [ns]: {
    apiVersion: 'tanka.dev/v1alpha1',
    kind: 'Environment',
    metadata: { name: ns },
    // 'default', not ns: Tanka injects spec.namespace into objects it does
    // not know are cluster-scoped (ClusterIssuer, ClusterImageCatalog,
    // IngressClass), and today's manifests carry 'default' there.
    spec: { apiServer: '', namespace: 'default' },
    data: { [key]: lab[key] for key in std.objectFields(namespaceOf) if namespaceOf[key] == ns },
  }
  for ns in namespaces
}
