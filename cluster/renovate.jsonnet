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
  // Mirror the flake.lock posture (.github/workflows/update-flake-lock.yml):
  // anything short of a major merges itself once the required `build` check
  // passes. These updates are strictly narrower than a nixpkgs bump, which
  // has auto-merged unattended since #65.
  //
  // platformAutomerge hands the merge to GitHub rather than Renovate, which
  // only wakes at 04:00 UTC -- without it a PR going green at 04:10 would
  // wait ~24h for the next run. Same mechanism as the flake path's
  // `gh pr merge --auto`.
  platformAutomerge: true,
  packageRules: [
    {
      matchUpdateTypes: ['minor', 'patch', 'digest'],
      automerge: true,
    },
    {
      // vectorchord tags are '<postgres-version>-<vchord-version>', e.g.
      // '16.9-0.4.3'. Renovate's default docker versioning reads everything
      // after the '-' as an immutable compatibility suffix (as it would for
      // '-alpine'), so it only ever offers tags carrying our exact '-0.4.3'.
      // The newest of those is 17.5-0.4.3, which is why #78 proposed a
      // PostgreSQL 16 -> 17 major jump -- to a stale patch level, since the
      // PG 17 line is at 17.10 -- while pinning the extension at 0.4.3
      // forever. Upstream is on 1.1.1; no PR would ever have surfaced that,
      // because every extension bump changes the suffix.
      //
      // Parsing both halves puts the PG major in `major`, so a PG major
      // upgrade is correctly typed as major and extension updates become
      // visible at all.
      matchPackageNames: ['ghcr.io/tensorchord/cloudnative-vectorchord'],
      versioning: 'regex:^(?<major>\\d+)\\.(?<minor>\\d+)-(?<patch>\\d+)\\.(?<build>\\d+)\\.(?<revision>\\d+)$',
      // Never unattended: the tag couples a PostgreSQL major version with the
      // extension version. A CNPG imageName change across PG majors triggers
      // an offline in-place pg_upgrade, and an extension major (0.4 -> 1.1)
      // needs its own ALTER EXTENSION step. Neither is judgeable from the tag,
      // so this always goes through triage review.
      automerge: false,
    },
  ],
  postUpgradeTasks: {
    commands: ['cd cluster && tk tool charts vendor --prune', 'cd cluster && just render-lab'],
    fileFilters: ['cluster/charts/**', 'cluster/manifests/**'],
  },
}
