# Changes fleet-agent.service itself: the switch it triggers must complete.
{systemd.services.fleet-agent.environment.FLEET_TEST = "agent-change";}
