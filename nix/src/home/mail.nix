{ config, pkgs, ... }:

let
  secrets = import ../secrets.nix;
in {
  accounts.email.maildirBasePath = ".maildir";

  programs.mbsync.enable = true;
  programs.msmtp.enable = true;

  xdg.configFile."mbsync/postExec" = {
    text = ''
      #!${pkgs.stdenv.shell}
      ${pkgs.procps}/bin/pkill mu
      sleep 0.1
      ${pkgs.mu}/bin/mu index --maildir=.maildir
    '';
    executable = true;
  };
  services.mbsync = {
    enable = true;
    postExec = "${config.xdg.configHome}/mbsync/postExec";
  };

  accounts.email.accounts.fitzmattd = {
    userName = "fitz.matt.d@gmail.com";
    primary = true;
    address = "fitz.matt.d@gmail.com";
    mbsync = {
      enable = true;
      create = "both";
      expunge = "both";
      remove = "both";
    };
    passwordCommand = "echo ${secrets.email.fitzmattd}";
    imap = {
      host = "imap.gmail.com";
    };
  };

  #accounts.email.accounts.fitzsimmonsio = {
  #  userName = "matthew@fitzsimmons.io";
  #  address = "matthew@fitzsimmons.io";
  #  mbsync = {
  #    enable = true;
  #    create = "both";
  #    expunge = "both";
  #    remove = "both";
  #  };
  #  msmtp = {
  #    enable = true;
  #  };
  #  passwordCommand = "echo ${secrets.email.fitzsimmonsio}";
  #  imap = {
  #    host = "127.0.0.1";
  #    port = 1143;
  #    tls.useStartTls = true;
  #    tls.certificatesFile = ~/.config/protonmail/bridge/cert.pem;
  #  };
  #  smtp = {
  #    host = "127.0.0.1";
  #    port = 1025;
  #    tls = {
  #      enable = true;
  #      useStartTls = true;
  #      certificatesFile = ~/.config/protonmail/bridge/cert.pem;
  #    };
  #  };
  #};

  accounts.email.accounts.ftzm = {
    userName = "m@ftzm.org";
    address = "m@ftzm.org";
    mbsync = {
      enable = true;
      create = "both";
      expunge = "both";
      remove = "both";
    };
    msmtp = {
      enable = true;
    };
    passwordCommand = "echo ${secrets.email.ftzm}";
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
  };
}
