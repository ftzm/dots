{
  pkgs,
  lib,
  ...
}: let
  mqtt2prometheus = pkgs.callPackage ./mqtt2prometheus.nix {};
  settingsFormat = pkgs.formats.yaml {};
  settings = {
    mqtt = {
      server = "tcp://0.0.0.0:1883";
      topic_path = "tele/+/SENSOR";
      device_id_regex = "tele/(?P<deviceid>.*)/SENSOR";
      qos = 0;
    };
    cache = {
      timeout = "24h";
    };
    metrics = [
      {
        prom_name = "consumed_energy_kilowatthours_total";
        mqtt_name = "ENERGY.Total";
        help = "total measured kilowatthours since flash";
        type = "counter";
      }
      {
        prom_name = "voltage_volts";
        mqtt_name = "ENERGY.Voltage";
        help = "Currently measured voltage";
        type = "gauge";
      }
      {
        prom_name = "current_amperes";
        mqtt_name = "ENERGY.Current";
        help = "Currently measured current";
        type = "gauge";
      }
      {
        prom_name = "power_watts";
        mqtt_name = "ENERGY.Power";
        help = "Currently measured power";
        type = "gauge";
      }
      {
        prom_name = "apparent_power_watt";
        mqtt_name = "ENERGY.ApparentPower";
        help = "Currently apparent power";
        type = "gauge";
      }
      {
        prom_name = "reactive_power_watt";
        mqtt_name = "ENERGY.ReactivePower";
        help = "Currently reactive power";
        type = "gauge";
      }
    ];
  };
  cfg = {
    listenAddress = "0.0.0.0";
    listenPort = 9641;
    logFormat = "console";
    logLevel = "info";
  };
in {
  systemd.services.mqtt2prometheus = {
    after = ["network.target"];
    wantedBy = ["multi-user.target"];
    description = "MQTT to Prometheus gateway";
    serviceConfig = let
      configFile = settingsFormat.generate "config.yaml" settings;
    in {
      ExecStart = ''
        ${mqtt2prometheus}/bin/mqtt2prometheus \
          -config ${configFile} \
          -listen-address ${lib.escapeShellArg cfg.listenAddress} \
          -listen-port ${toString cfg.listenPort} \
          -log-format ${cfg.logFormat} \
          -log-level ${cfg.logLevel}
      '';
      DynamicUser = "yes";
      PrivateUsers = "yes";
      PrivateDevices = "yes";
      ProtectClock = "yes";
      ProtectControlGroups = "yes";
      ProtectHome = "yes";
      ProtectKernelLogs = "yes";
      ProtectKernelModules = "yes";
      ProtectKernelTunables = "yes";
      ProtectHostname = "yes";
      ProtectProc = "noaccess";
      ProcSubset = "pid";
      LockPersonality = "yes";
      RestrictAddressFamilies = ["AF_INET" "AF_INET6"];
      RestrictRealtime = "yes";
      RestrictNamespaces = "yes";
      MemoryDenyWriteExecute = "yes";
      CapabilityBoundingSet = "";
      SystemCallArchitectures = "native";
      SystemCallFilter = ["@system-service" "~@privileged"];
      UMask = "0027";
    };
  };
}
