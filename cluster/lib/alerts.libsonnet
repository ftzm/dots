// Alert conventions, enforced in code rather than by memory.
//
// One rule DSL serves both delivery mechanisms:
//   - alerts.prometheusRule()  -> PrometheusRule CR (metric alerts)
//   - alerts.lokiRule()        -> ConfigMap labeled `loki_rule` (log alerts,
//                                 picked up by the loki chart's k8s-sidecar)
//
// Conventions enforced here:
//   - severity vocabulary (assert), mandatory summary/description
//   - the `release: kube-prometheus-stack` label the Prometheus ruleSelector
//     requires (un-forgettable)
//   - the `loki_rule` label the Loki sidecar watches (un-forgettable)
//
// Colocation: alerts live in the block of the thing they watch. Cross-cutting
// rules (journal, comin, node units, meta-alerts) live in the top-level
// `observability:` block in main.jsonnet.
local severities = ['critical', 'warning', 'info'];

{
  // A single alert. name is the alert name (CamelCase, Prometheus convention).
  // expr is PromQL or LogQL depending on the renderer used.
  rule(name, expr, duration, severity, summary, description):: {
    assert std.member(severities, severity) : name + ': invalid severity ' + severity,
    assert summary != '' : name + ': summary required',
    assert description != '' : name + ': description required',
    alert: name,
    expr: expr,
    'for': duration,
    labels: { severity: severity },
    annotations: { summary: summary, description: description },
  },

  // Metric alerts. Stamps the release label the ruleSelector matches on.
  prometheusRule(name, ns, rules):: {
    apiVersion: 'monitoring.coreos.com/v1',
    kind: 'PrometheusRule',
    metadata: {
      name: name,
      namespace: ns,
      labels: { release: 'kube-prometheus-stack' },
    },
    spec: {
      groups: [{ name: name, rules: rules }],
    },
  },

  // Log alerts (LogQL). Renders a ConfigMap the Loki k8s-sidecar watches
  // (label `loki_rule`, folder /rules).
  //
  // A LogQL alert must be a metric query. A bare log selector loads fine and
  // then fails every single evaluation ("rule result is not a vector or
  // scalar"), so the rule sits inactive forever while the failure it exists
  // to catch goes unreported -- two rules were dead this way for weeks. Every
  // LogQL metric query is built from a range aggregation, so requiring one
  // rejects that shape at render time instead of at 3am.
  lokiRule(name, ns, rules):: {
    assert std.all([
      std.length(std.findSubstr('_over_time(', r.expr)) > 0 ||
      std.length(std.findSubstr('rate(', r.expr)) > 0
      for r in rules
    ]) : name + ': every Loki alert expr must be a metric query (a range aggregation over the selector), not a bare log selector',

    apiVersion: 'v1',
    kind: 'ConfigMap',
    metadata: {
      name: 'loki-rule-' + name,
      namespace: ns,
      labels: { loki_rule: '1' },
      // Loki's local rule store enumerates SUBDIRECTORIES of
      // ruler.storage.local.directory as tenant IDs. With auth_enabled=false
      // the tenant is `fake`, so a file dropped at /rules/<name>.yaml is never
      // read and the rule silently never loads. The sidecar honours this
      // annotation to place the file under the tenant dir instead.
      annotations: { 'k8s-sidecar-target-directory': '/rules/fake' },
    },
    data: {
      [name + '.yaml']: std.manifestYamlDoc({
        groups: [{ name: name, rules: rules }],
      }),
    },
  },

  // A ServiceMonitor scraping /metrics on a named port every 30s.
  serviceMonitor(name, ns, matchLabels, port):: {
    apiVersion: 'monitoring.coreos.com/v1',
    kind: 'ServiceMonitor',
    metadata: { name: name, namespace: ns },
    spec: {
      selector: { matchLabels: matchLabels },
      endpoints: [{
        port: port,
        path: '/metrics',
        interval: '30s',
      }],
    },
  },
}
