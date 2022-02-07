{ config, pkgs, ... }:

{
  accounts.email.maildirBasePath = ".maildir";

  programs.mbsync.enable = true;
  programs.msmtp.enable = true;
  programs.mu.enable = true;

  xdg.configFile."mbsync/postExec" = {
    text = with pkgs;''
      #!${stdenv.shell}
      ${procps}/bin/pkill mu
      ${coreutils}/bin/sleep 0.1
      # [ ! -d ~/.cache/mu ] && ${mu}/bin/mu init --maildir=.maildir
      ${mu}/bin/mu index
    '';
    executable = true;
  };
  services.mbsync = {
    enable = true;
    postExec = "${config.xdg.configHome}/mbsync/postExec";
  };
  accounts.email.accounts.fitzmattd = {
    userName = "fitz.matt.d@gmail.com";
    address = "fitz.matt.d@gmail.com";
    mbsync = {
      enable = true;
      create = "both";
      expunge = "both";
      remove = "both";
      extraConfig.channel = {
        "CopyArrivalDate" = "yes";
      };
    };
    mu.enable = true;
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
      extraConfig.channel = {
        "CopyArrivalDate" = "yes";
      };
    };
    mu.enable = true;
    msmtp = {
      enable = true;
    };
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
