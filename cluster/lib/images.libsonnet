{
  radarr: 'linuxserver/radarr:6.3.0',
  sonarr: 'linuxserver/sonarr:4.0.19',
  lidarr: 'linuxserver/lidarr:3.1.3-nightly',
  readarr: 'linuxserver/readarr:0.4.19-nightly',
  prowlarr: 'linuxserver/prowlarr:2.5.1-nightly',
  flaresolverr: 'flaresolverr/flaresolverr:v3.5.0',
  jellyseerr: 'fallenbagel/jellyseerr:develop',
  vaultwarden: 'vaultwarden/server:1.36.0',
  forgejo: 'codeberg.org/forgejo/forgejo:15.0.3',
  // Immich's database. Both majors stay listed while the 16 -> 18 upgrade is
  // in flight: CloudNativePG's rollback for a failed pg_upgrade is to put the
  // Cluster's major back, and that only resolves while the catalog still
  // offers the old one. Drop the 16 entry once 18 has proven itself.
  cloudnativeVectorchord16: 'ghcr.io/tensorchord/cloudnative-vectorchord:16.14-1.1.1',
  cloudnativeVectorchord18: 'ghcr.io/tensorchord/cloudnative-vectorchord:18.4-1.1.1',
  blocky: 'spx01/blocky:latest',
  ntfy: 'binwiederhier/ntfy',
  navidrome: 'ghcr.io/navidrome/navidrome:0.61.2',
  audiobookshelf: 'advplyr/audiobookshelf:2.35.1',
  thelounge: 'thelounge/thelounge:4.5.2',
  filestash: 'machines/filestash:latest',
  pinepods: 'madeofpendletonwool/pinepods:0.9.0',
  miniflux: 'miniflux/miniflux:2.3.3',
  valkey: 'valkey/valkey:9-alpine',
  cnpgPostgres: 'ghcr.io/cloudnative-pg/postgresql:18.2',
}
