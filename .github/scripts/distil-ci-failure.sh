#!/usr/bin/env bash
#
# Turn a `gh run view --log-failed` dump into something worth putting in a
# prompt, for the repair agents in ../workflows/auto-fix-flake-update.yml and
# ../workflows/renovate-triage.yml.
#
# These logs are ~20k lines and mostly chatter: one line per derivation built
# or copied, plus magic-nix-cache throttling that prints "error: unable to
# download ... HTTP error 418" hundreds of times and is never the cause. The
# auto-fix run on #147 spent all 40 of its turns grepping past exactly that
# and died at the turn ceiling without reaching a fix. So the noise is dropped
# here, once, deterministically, instead of being rediscovered per run.
#
# Usage: distil-ci-failure.sh RAW_LOG OUT_DIR
#
# Writes into OUT_DIR:
#   failed.full.log   the whole log, de-prefixed
#   failed.quiet.log  chatter removed
#   failed.log        tail of failed.full.log, bounded for tokens
#   diagnosis.log     the failure itself: matched errors with their context

set -euo pipefail

if [ "$#" -ne 2 ]; then
  echo "usage: $(basename "$0") RAW_LOG OUT_DIR" >&2
  exit 2
fi

raw=$1
out=$2
mkdir -p "$out"

# What counts as the failure. Nix says "error:", helm and the gh CLI say
# "Error:", jsonnet says "RUNTIME ERROR", just says "Recipe ... failed", and
# the flake build prints ❌ against the attribute that could not be built.
readonly SIGNAL='error: |Error: |ERROR|hash mismatch|builder for .* failed|Cannot build |last 10 log lines|assertion .* failed|attribute .* missing|Recipe .* failed|panic: |##\[error\]|^❌'

# Chatter. Each pattern here is output that appears whether or not the run
# failed; none of it has ever been the reason one did.
readonly NOISE="^(building |copying path |\s+/nix/store/|Post job cleanup|\[command\]|##\[(start-action|end-action|group|endgroup))\
|narinfo|HTTP error 418|rate limit exceeded|Twirp|magic.nix.cache|Magic Nix Cache\
|substituter '[^']*' is disabled|Node(\.js)? 20 (is|are) (being )?deprecat"

# Every line arrives as "job<TAB>step<TAB>timestamp<TAB>text", some with a
# UTF-8 BOM wedged in front of the timestamp; strip both so what follows sees
# plain build output.
tr -d '\357\273\277' < "$raw" \
  | sed -E 's/^[^\t]*\t[^\t]*\t[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9:.]+Z ?//' \
  > "$out/failed.full.log"

# Drop the chatter before matching, so the context lines around each error are
# themselves worth reading.
grep -avE "$NOISE" "$out/failed.full.log" > "$out/failed.quiet.log" || true

grep -aE -B 2 -A 15 "$SIGNAL" "$out/failed.quiet.log" \
  | head -300 | head -c 12000 > "$out/diagnosis.log" || true

# Full-log fallback for the agent, bounded for tokens.
tail -c 200000 "$out/failed.full.log" > "$out/failed.log"

wc -l "$out/failed.log" "$out/diagnosis.log"
