// One file per image under images/, so two Renovate bumps never edit
// adjacent lines of one file: a bump PR can only fall behind master, never
// conflict with a sibling.
{
  radarr: import 'images/radarr.libsonnet',
  sonarr: import 'images/sonarr.libsonnet',
  lidarr: import 'images/lidarr.libsonnet',
  readarr: import 'images/readarr.libsonnet',
  prowlarr: import 'images/prowlarr.libsonnet',
  cleanuparr: import 'images/cleanuparr.libsonnet',
  flaresolverr: import 'images/flaresolverr.libsonnet',
  jellyseerr: import 'images/jellyseerr.libsonnet',
  vaultwarden: import 'images/vaultwarden.libsonnet',
  forgejo: import 'images/forgejo.libsonnet',
  cloudnativeVectorchord18: import 'images/cloudnativeVectorchord18.libsonnet',
  blocky: import 'images/blocky.libsonnet',
  ntfy: import 'images/ntfy.libsonnet',
  navidrome: import 'images/navidrome.libsonnet',
  audiobookshelf: import 'images/audiobookshelf.libsonnet',
  thelounge: import 'images/thelounge.libsonnet',
  pinepods: import 'images/pinepods.libsonnet',
  miniflux: import 'images/miniflux.libsonnet',
  valkey: import 'images/valkey.libsonnet',
  cnpgPostgres: import 'images/cnpgPostgres.libsonnet',
  healthchecks: import 'images/healthchecks.libsonnet',
  alpineK8s: import 'images/alpineK8s.libsonnet',
}
