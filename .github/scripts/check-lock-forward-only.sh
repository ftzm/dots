#!/usr/bin/env bash
#
# Guard the flake.lock edits made by the auto-fix agent (see
# ../workflows/auto-fix-flake-update.yml).
#
# The agent is allowed to touch flake.lock, because the most common way an
# automated flake update breaks is an input pinned mid-breakage upstream --
# the fix is to move that input further forward, not to patch our config.
# The move that must never happen is the opposite one: quietly rolling the
# update back so CI goes green and auto-merge lands a no-op. This script is
# what makes that distinction mechanical rather than a line in a prompt.
#
# Per input node, comparing the repair branch against the PR being repaired:
#
#   forward    lastModified is newer  -> allowed
#   hold-back  rev is exactly the pre-update pin, i.e. the input is dropped
#              from this update -> allowed, and reported
#   backwards  anything else -> violation
#
# Reverting *every* input the PR moved is also a violation: that is not a
# repair, it is a silent cancellation of the update.
#
# Node additions and removals are reported but never fail: moving an input
# forward legitimately changes its own transitive input set.
#
# Usage: check-lock-forward-only.sh BASE_LOCK FIX_LOCK PREUPDATE_LOCK
#   BASE_LOCK       flake.lock on the flake-update PR (what CI failed on)
#   FIX_LOCK        flake.lock on the repair branch
#   PREUPDATE_LOCK  flake.lock the update was cut from (merge base with master)

set -euo pipefail

if [ "$#" -ne 3 ]; then
  echo "usage: $(basename "$0") BASE_LOCK FIX_LOCK PREUPDATE_LOCK" >&2
  exit 2
fi

base=$1
fix=$2
pre=$3

for f in "$base" "$fix" "$pre"; do
  [ -r "$f" ] || { echo "::error::lock file not readable: $f"; exit 2; }
done

report=$(jq -n -r \
  --slurpfile base "$base" \
  --slurpfile fix "$fix" \
  --slurpfile pre "$pre" '
  def ident: (.locked.rev // .locked.narHash // "");
  def stamp: (.locked.lastModified // 0);
  def short: if length > 9 then .[0:9] else . end;

  ($base[0].nodes) as $b | ($fix[0].nodes) as $f | ($pre[0].nodes) as $p |

  # Inputs this PR actually moved -- the update whose survival we are checking.
  [ $b | keys_unsorted[]
    | select($b[.].locked != null and $p[.].locked != null)
    | select(($b[.]|ident) != ($p[.]|ident)) ] as $updated |

  [ $f | keys_unsorted[]
    | select($f[.].locked != null and $b[.] != null and $b[.].locked != null)
    | . as $k
    | ($b[$k]) as $bn | ($f[$k]) as $fn
    | select(($bn|ident) != ($fn|ident))
    | if ($fn|stamp) > ($bn|stamp) then
        { level: "ok",
          key: $k,
          msg: "\($k): forward \(($bn|ident)|short) -> \(($fn|ident)|short)" }
      elif ($p[$k] != null and ($p[$k]|ident) == ($fn|ident)) then
        { level: "holdback",
          key: $k,
          msg: "\($k): held back to the pre-update pin \(($fn|ident)|short) -- dropped from this update" }
      else
        { level: "violation",
          key: $k,
          msg: "\($k): moved backwards to \(($fn|ident)|short) (\(($fn|stamp)) is not newer than the PR pin \(($bn|ident)|short) (\(($bn|stamp))), and is not the pre-update pin)" }
      end ] as $changes |

  ($changes | map(select(.level == "holdback") | .key)) as $held |

  $changes
  + [ $b | keys_unsorted[] | select($f[.] == null)
      | { level: "note", key: ., msg: "\(.): input no longer in the lock" } ]
  + [ $f | keys_unsorted[] | select($b[.] == null)
      | { level: "note", key: ., msg: "\(.): input new in the lock" } ]
  + ( if ($updated | length) > 0 and (($updated - $held) | length) == 0 then
        [ { level: "violation",
            key: "*",
            msg: "every input this PR updated (\($updated | join(", "))) was held back -- the repair cancels the update instead of fixing it" } ]
      else [] end )
  | .[] | "\(.level)\t\(.msg)"
  ')

if [ -z "$report" ]; then
  echo "flake.lock is untouched by the repair"
  exit 0
fi

echo "$report" | while IFS=$'\t' read -r level msg; do
  case "$level" in
    violation) echo "  BACKWARDS  $msg" ;;
    holdback)  echo "  HOLD-BACK  $msg" ;;
    ok)        echo "  FORWARD    $msg" ;;
    *)         echo "  note       $msg" ;;
  esac
done

if echo "$report" | grep -q '^violation'; then
  echo "::error::the repair moves flake.lock inputs backwards; see the BACKWARDS lines above"
  exit 1
fi

echo "all flake.lock changes move forward or are explicit hold-backs"
