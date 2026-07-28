# Sorts Fastmail into folders and ages out old bulk mail, via imapfilter.
#
# Sieve can't do this job for two independent reasons: Fastmail exposes no API
# to install a script (no ManageSieve, and urn:ietf:params:jmap:sieve is
# unimplemented on their JMAP endpoint), and Sieve runs at delivery so it has
# no concept of a message's age -- which the retention pass depends on.
#
# Rules live in ./mailsort.lua. Dry run before trusting it:
#
#   mkdir -p ~/.config/mailsort
#   sudo cp /run/agenix/ftzm-org-email ~/.config/mailsort/imap-password
#   imapfilter -n -c role/mailsort.lua
{
  config,
  lib,
  pkgs,
  ...
}: {
  # Reuses the Fastmail app password that mbsync uses. mkDefault so a machine
  # importing role/mail.nix too -- which declares this secret with
  # owner = "ftzm" -- wins rather than hitting a conflicting definition.
  age.secrets.ftzm-org-email.file = lib.mkDefault ../secrets/ftzm-org-email.age;

  systemd.services.mailsort = {
    description = "Sort Fastmail into folders and age out old bulk mail";
    after = ["network-online.target"];
    wants = ["network-online.target"];

    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${pkgs.imapfilter}/bin/imapfilter -c ${./mailsort.lua}";

      # agenix writes secrets root-owned 0400, and DynamicUser has no stable
      # uid to chown to. LoadCredential has systemd read it as root and hand
      # it to the process alone, so the two compose without loosening either.
      LoadCredential = "imap-password:${config.age.secrets.ftzm-org-email.path}";

      Environment = [
        # imapfilter only writes here if it has to accept an unverifiable
        # certificate, which it shouldn't -- but it builds the path
        # unconditionally, so give it a throwaway one. PrivateTmp makes this a
        # per-run tmpfs that is discarded afterwards.
        "HOME=/tmp"
        "SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
      ];

      # Stateless by design: every run re-derives its sets from the live
      # mailbox, so there is nothing to own and nothing to persist between
      # runs -- which is what makes DynamicUser free here.
      DynamicUser = true;
      ProtectSystem = "strict";
      ProtectHome = true;
      PrivateTmp = true;
      PrivateDevices = true;
      NoNewPrivileges = true;
      RestrictAddressFamilies = ["AF_INET" "AF_INET6"];
      RestrictNamespaces = true;
      LockPersonality = true;
      MemoryDenyWriteExecute = true;
      SystemCallFilter = ["@system-service"];
      SystemCallArchitectures = "native";
    };
  };

  systemd.timers.mailsort = {
    description = "Sort Fastmail every minute";
    wantedBy = ["timers.target"];
    timerConfig = {
      OnBootSec = "1min";
      OnUnitActiveSec = "1min";
      # Default accuracy is 1min, which would smear a 1min cadence badly.
      AccuracySec = "5s";
    };
  };
}
