local chartfile = std.parseYaml(importstr 'chartfile.yaml');

// Build a repo name -> URL lookup
local repoUrls = {
  [repo.name]: repo.url
  for repo in chartfile.repositories
};

// Generate a customManager per chart
local managers = [
  {
    customType: 'regex',
    managerFilePatterns: ['/chartfile\\.yaml$/'],
    matchStrings: [
      '- chart: %s\\s+version: "?(?<currentValue>[^"\\s]+)"?' % req.chart,
    ],
    depNameTemplate: std.split(req.chart, '/')[1],
    datasourceTemplate: 'helm',
    registryUrlTemplate: repoUrls[std.split(req.chart, '/')[0]],
  }
  for req in chartfile.requires
];

{
  '$schema': 'https://docs.renovatebot.com/renovate-schema.json',
  extends: ['config:recommended'],
  customManagers: managers,
  enabledManagers: ['custom.regex', 'github-actions', 'jsonnet-bundler'],
  prHourlyLimit: 10,
  prConcurrentLimit: 20,
  postUpgradeTasks: {
    commands: ['tk tool charts vendor --prune', 'just render-lab'],
    fileFilters: ['charts/**', 'manifests/**'],
  },
}
