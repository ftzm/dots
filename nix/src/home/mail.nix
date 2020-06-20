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
      ${pkgs.bash}/bin/sleep 0.1
      [ ! -d ~/.cache/mu ] && ${pkgs.mu}/bin/mu init --maildir=.maildir
      ${pkgs.mu}/bin/mu index
    '';
    executable = true;
  };
  services.mbsync = {
    enable = true;
    postExec = "${config.xdg.configHome}/mbsync/postExec";
  };

  accounts.email.accounts.unity = {
    userName = "matthew.fitzsimmons@unity3d.com";
    address = "matthew.fitzsimmons@unity3d.com";
    mbsync = {
      enable = true;
      create = "both";
      expunge = "both";
      remove = "both";
    };
    passwordCommand = "echo ${secrets.email.unity}";
    imap = {
      host = "imap.gmail.com";
    };
  };

  accounts.email.accounts.fitzmattd = {
    userName = "fitz.matt.d@gmail.com";
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

  accounts.email.accounts.ftzm = {
    userName = "m@ftzm.org";
    address = "m@ftzm.org";
    primary = true;
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
