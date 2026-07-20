local chartfile = std.parseYaml(importstr 'chartfile.yaml');

// Build a repo name -> URL lookup
local repoUrls = {
  [repo.name]: repo.url
  for repo in chartfile.repositories
};

// Generate a customManager per chart
local chartManagers = [
  {
    customType: 'regex',
    managerFilePatterns: ['/cluster/chartfile\\.yaml$/'],
    matchStrings: [
      '- chart: %s\\s+version: "?(?<currentValue>[^"\\s]+)"?' % req.chart,
    ],
    depNameTemplate: std.split(req.chart, '/')[1],
    datasourceTemplate: 'helm',
    registryUrlTemplate: repoUrls[std.split(req.chart, '/')[0]],
  }
  for req in chartfile.requires
];

// Container images in images.libsonnet
local imageManager = {
  customType: 'regex',
  managerFilePatterns: ['/cluster/lib/images\\.libsonnet$/'],
  matchStrings: [
    "'(?<depName>[a-z0-9._/-]+):(?<currentValue>[^']+)'",
  ],
  datasourceTemplate: 'docker',
};

// Forgejo Actions runner image, pinned in the microVM guest module on nuc.
// Kept on the same Renovate flow as the cluster images so the runner version
// tracks with the rest of the fleet (comin deploys it, not ArgoCD).
local runnerImageManager = {
  customType: 'regex',
  managerFilePatterns: ['/machines/nuc/forgejo-runner\\.nix$/'],
  matchStrings: [
    'runnerImage = "(?<depName>[a-z0-9._/-]+):(?<currentValue>[^"]+)"',
  ],
  datasourceTemplate: 'docker',
};

{
  '$schema': 'https://docs.renovatebot.com/renovate-schema.json',
  extends: ['config:recommended'],
  customManagers: chartManagers + [imageManager, runnerImageManager],
  enabledManagers: ['custom.regex', 'github-actions', 'jsonnet-bundler'],
  prHourlyLimit: 10,
  prConcurrentLimit: 20,
  postUpgradeTasks: {
    commands: ['cd cluster && tk tool charts vendor --prune', 'cd cluster && just render-lab'],
    fileFilters: ['cluster/charts/**', 'cluster/manifests/**'],
  },
}
