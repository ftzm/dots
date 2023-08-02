{ config, pkgs, ... }:

{
  age.secrets = {
    fitzmattd-email = {
      file = ../secrets/fitzmattd-email.age;
      owner = "ftzm";
    };
    ftzm-org-email = {
      file = ../secrets/ftzm-org-email.age;
      owner = "ftzm";
    };
  };

  home-manager.users.ftzm = {
    accounts.email.maildirBasePath = ".maildir";

    programs.mbsync.enable = true;
    services.mbsync = {
      enable = true;
      postExec = "${config.home-manager.users.ftzm.xdg.configHome}/mbsync/postExec";
    };
    xdg.configFile."mbsync/postExec" = {
      text = with pkgs; ''
        #!${stdenv.shell}
        ${procps}/bin/pkill -x mu
        ${coreutils}/bin/sleep 0.1
        # [ ! -d ~/.cache/mu ] && ${mu}/bin/mu init --maildir=.maildir
        ${mu}/bin/mu index
      '';
      executable = true;
    };

    # programs.msmtp.enable = true;
    programs.mu.enable = true;

    accounts.email.accounts.fitzmattd = {
      userName = "fitz.matt.d@gmail.com";
      address = "fitz.matt.d@gmail.com";
      mbsync = {
        enable = true;
        create = "both";
        expunge = "both";
        remove = "both";
        extraConfig.channel = { "CopyArrivalDate" = "yes"; };
      };
      mu.enable = true;
      imap = { host = "imap.gmail.com"; };
      passwordCommand =
        "${pkgs.coreutils}/bin/cat ${config.age.secrets.fitzmattd-email.path}";
    };

    accounts.email.accounts.ftzm = {
      userName = "m@ftzm.org";
      address = "m@ftzm.org";
      primary = true;
      mbsync = {
        enable = true;
        create = "both";
        expunge = "both";
        remove = "both";
        extraConfig.channel = { "CopyArrivalDate" = "yes"; };
      };
      mu.enable = true;
      # msmtp = {
      #   enable = true;
      # };
      imap = {
        host = "imap.fastmail.com";
        port = 993;
        tls.enable = true;
        tls.useStartTls = false;
      };
      smtp = {
        host = "smtp.fastmail.com";
        port = 465;
        tls = {
          enable = true;
          useStartTls = false;
        };
      };
      passwordCommand =
        "${pkgs.coreutils}/bin/cat ${config.age.secrets.ftzm-org-email.path}";
    };
  };
}
