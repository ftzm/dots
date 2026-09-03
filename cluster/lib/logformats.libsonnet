// The log format -> parser map, as data. Phase 1's single source of truth:
//
//   - renderProcess() generates the alloy `loki.process "default"` River block
//     (one stage.match per format below, plus level normalization and the
//     coverage signal).
//   - renderDocs() generates the markdown table documenting the same map.
//
// A new app or parser is one entry here — never a hand-edited River block, and
// the docs can't drift from the config. The coverage signal (level="unknown")
// is emitted unconditionally, so a parser can't be added without its guard.
//
// Selectors match on stream labels alloy already sets (namespace/app from pod
// discovery). The mapping is seeded from the 2026-08 audit's format inventory
// and is expected to be tuned against the level="unknown" fraction in the
// deploy loop — the coverage signal is exactly how you find the misses.
{
  formats: [
    // Selectors MUST be mutually exclusive. Stages run in order and a later
    // stage.regex overwrites a level an earlier stage.json already extracted
    // correctly, so an overlap is a false-positive generator, not a no-op.
    {
      name: 'json',
      kind: 'json',
      // Verified JSON in the 2026-08 audit: traefik, cnpg, miniflux.
      // nextcloud/homepage/pinepods/atuin are unverified guesses — the
      // level="unknown" coverage signal is how they get corrected.
      selector: '{namespace=~"traefik|miniflux|cnpg-system|postgres|nextcloud|homepage|pinepods|atuin"}',
      json: { level: 'level', msg: 'msg', message: 'message' },
    },
    {
      name: 'json-immich',
      kind: 'json',
      // immich-postgres is JSON; valkey in the same namespace is not, so this
      // is split by container rather than namespace.
      selector: '{namespace="immich", container!~"valkey|redis"}',
      json: { level: 'level', msg: 'msg', message: 'message' },
    },
    {
      name: 'logfmt',
      kind: 'logfmt',
      // argocd, loki, alloy, prometheus. kube-system is NOT logfmt (coredns
      // is bracket format) — it belongs to the bracket entry below.
      selector: '{namespace=~"argocd|monitoring"}',
      logfmt: { level: 'level', msg: 'msg', message: 'message' },
    },
    {
      name: 'bracket',
      kind: 'regex',
      // [timestamp] LEVEL text — blocky, media/arr stack, vaultwarden, coredns.
      selector: '{namespace=~"media|vaultwarden|kube-system|blocky"}',
      regex: '^\\[[^\\]]*\\]\\s*\\[(?P<level>(DEBUG|INFO|WARN|WARNING|ERROR|FATAL))\\]',
    },
    {
      name: 'prefix-info',
      kind: 'regex',
      // "timestamp INFO text" — ntfy.
      selector: '{namespace="ntfy"}',
      regex: '^(?P<level>(DEBUG|INFO|WARN|WARNING|ERROR|FATAL))\\b',
    },
    {
      name: 'gitea',
      kind: 'regex',
      // "[I]"/"[W]"/"[E]" — forgejo/gitea.
      selector: '{namespace=~"gitea|forgejo"}',
      regex: '^\\[(?P<level>[IWEF])\\]',
    },
    {
      name: 'redis',
      kind: 'regex',
      // Redis-style (immich valkey). Anchored to the line-leading Redis log
      // shape rather than a bare word match, so the word ERROR appearing in a
      // message body cannot mint a false level.
      selector: '{namespace="immich", container=~"valkey|redis"}',
      regex: '^[^ ]+ +[0-9]+ [^ ]+ +[0-9:.]+ (?P<level>[.\\-*#])',
    },
  ],

  // Canonical level: lowercase, aliases folded (WARNING/W->warn, I->info),
  // empty -> unknown (the coverage signal). Runs for every format so the
  // schema stays uniform. $lvl is a Go-template variable (alloy templates are
  // text/template + sprig, same as the original ToLower pipeline).
  // Canonical level: lowercase, aliases folded, and CLAMPED to the canonical
  // set — anything else becomes `unknown`. The clamp is load-bearing: `level`
  // is promoted to a Loki label for every stream, so a passthrough branch
  // would let an unexpected value (a numeric level from a JSON logger, a
  // stray regex capture) mint unbounded label cardinality. Redis single-char
  // markers (. - * #) are folded here too. $lvl is a Go-template variable.
  levelNormalization:
    '{{ $lvl := ToLower .Value }}' +
    '{{ if or (eq $lvl "debug") (eq $lvl ".") }}debug' +
    '{{ else if or (eq $lvl "info") (eq $lvl "i") (eq $lvl "notice") (eq $lvl "normal") (eq $lvl "-") (eq $lvl "*") }}info' +
    '{{ else if or (eq $lvl "warn") (eq $lvl "warning") (eq $lvl "w") (eq $lvl "#") }}warn' +
    '{{ else if or (eq $lvl "error") (eq $lvl "err") (eq $lvl "e") }}error' +
    '{{ else if or (eq $lvl "fatal") (eq $lvl "f") (eq $lvl "crit") (eq $lvl "alert") (eq $lvl "emerg") }}fatal' +
    '{{ else }}unknown{{ end }}',

  // River object literal from a map (`a = "b"`, not JSON `"a": "b"`).
  riverObject(m):: '{ %s }' % std.join(', ', [k + ' = ' + std.manifestJson(m[k]) for k in std.objectFields(m)]),

  // The alloy chart renders configMap.content through helm `tpl`, so any
  // literal `{{ }}` in the River config must be escaped as Go-template output
  // of a brace — the same `{{ "{{" }}` trick the original heredoc used. Two
  // passes with markers so the substitutions never re-trigger each other.
  helmEscape(s)::
    local a = std.strReplace(s, '{{', 'ZZ_LCB_ZZ');
    local b = std.strReplace(a, '}}', 'ZZ_RCB_ZZ');
    std.strReplace(
      std.strReplace(b, 'ZZ_LCB_ZZ', '{{ "{{" }}'),
      'ZZ_RCB_ZZ', '{{ "}}" }}'
    ),

  renderProcess(name='default')::
    local stages = std.join('\n', std.map(function(f)
      local extract =
        if f.kind == 'json' then
          '    stage.json { expressions = ' + self.riverObject(f.json) + ' }\n'
        else if f.kind == 'logfmt' then
          '    stage.logfmt { mapping = ' + self.riverObject(f.logfmt) + ' }\n'
        else
          '    stage.regex { expression = ' + std.manifestJson(f.regex) + ' }\n';
      '  stage.match {\n' +
      '    selector = ' + std.manifestJson(f.selector) + '\n' +
      extract +
      '  }\n',
    self.formats));
    local content =
      'loki.process "%s" {\n' % name +
      '  forward_to = [loki.write.default.receiver]\n' +
      stages +
      '  stage.template {\n' +
      '    source   = "level"\n' +
      '    template = ' + std.manifestJson(self.levelNormalization) + '\n' +
      '  }\n' +
      '  stage.labels {\n' +
      '    values = { level = "" }\n' +
      '  }\n' +
      '}\n';
    self.helmEscape(content),

  // Journal and syslog bypass loki.process (they are not pod logs), so their
  // level is normalized HERE, at relabel time, from the priority/severity
  // keyword. Without this the schema is split: pod logs say `error`/`warn`
  // while the journal says `err`/`warning`/`notice`, and `{level="error"}`
  // silently misses every host log.
  //
  // Prometheus relabel semantics: `replace` is a no-op unless the anchored
  // regex matches, and later rules overwrite earlier ones — so the catch-all
  // runs first and the specific maps override it. An unmatched keyword lands
  // on `unknown`, which is the coverage signal, not a silent drop.
  // Complete `discovery.relabel` block for a host log source. Emitted whole
  // (rather than spliced mid-block) because a jsonnet ||| block takes its
  // indentation prefix from its first content line and every later line must
  // meet it — splicing inside the block breaks that invariant.
  hostRelabel(name, severityLabel, rules)::
    'discovery.relabel "' + name + '" {\n' +
    '  targets = []\n' +
    std.join('', [
      '  rule {\n' +
      '    source_labels = ["' + r[0] + '"]\n' +
      '    target_label  = "' + r[1] + '"\n' +
      '  }\n'
      for r in rules
    ]) +
    self.hostLevelRelabel(severityLabel) + '\n' +
    '}\n',

  hostLevelRelabel(sourceLabel)::
    local maps = [
      ['.*', 'unknown'],
      ['debug', 'debug'],
      ['info|notice', 'info'],
      ['warning|warn', 'warn'],
      ['err|error', 'error'],
      ['crit|alert|emerg', 'fatal'],
    ];
    std.join('\n', [
      '  rule {\n' +
      '    source_labels = ["' + sourceLabel + '"]\n' +
      '    regex         = "' + m[0] + '"\n' +
      '    target_label  = "level"\n' +
      '    replacement   = "' + m[1] + '"\n' +
      '  }'
      for m in maps
    ]),

  // Markdown table of the same data — Phase 4's documentation, generated.
  renderDocs()::
    local h = '| Format | Kind | Selector |\n|---|---|---|\n';
    h + std.join('\n', std.map(function(f)
      '| %s | %s | `%s` |' % [f.name, f.kind, f.selector],
    self.formats)),
}
