# mailsort

Inbox zero of mail that actually needs you. Low-value mail is filed into
folders at arrival, and bulk folders age out to Archive after 30 days.

- `mailsort.nix` — service + 1-minute timer, imported by `machines/nuc`
- `mailsort.lua` — the rules, for imapfilter

## Why imapfilter and not Sieve

Sieve can't do this job, for two independent reasons:

1. **Fastmail exposes no API to install a Sieve script.** No ManageSieve
   ("Fastmail does not support the 'managesieve' extension… the only way to
   modify your script is by logging in to the web interface"), and
   `urn:ietf:params:jmap:sieve` is unimplemented on their JMAP endpoint even
   though a Fastmail engineer authored RFC 9661. Every Sieve solution has a
   permanent manual step.
2. **Sieve runs at delivery and has no concept of a message's age.** The
   retention pass is unexpressable in it at any price.

imapfilter is in nixpkgs, has done this since ~2001, evaluates its searches
server-side, and has a real `-n` dry-run mode.

## Setup

The credential is the existing Fastmail app password that mbsync uses,
`secrets/ftzm-org-email.age`. `nuc` was added to its recipients, so it must be
rekeyed once before the NUC can decrypt it:

    cd secrets && agenix -r    # agenix resolves secrets.nix relative to $PWD,
                               # so run it from secrets/, not the repo root

agenix writes it root-owned 0400 and `LoadCredential` has systemd read it as
root and hand it to the service — which is what lets `DynamicUser` stay on,
since a dynamic uid can't own the file.

Dry run:

    mkdir -p ~/.config/mailsort
    cd secrets && agenix -d ftzm-org-email.age -i ~/.ssh/id_rsa \
      > ~/.config/mailsort/imap-password
    imapfilter -n -c role/mailsort.lua

## Design

| Folder | What lands there | Ages out |
|---|---|---|
| Promotions | Marketing: retail, travel/loyalty, service | 30d → Archive |
| Orders | Carriers, e-commerce platforms, order/shipping subjects | 30d → Archive |
| Property | Boligsiden and estate-agent saved-search alerts | 30d → Archive |
| Newsletters | Sampson Boat Co, Forbrugerrådet, Haskell Weekly, Substack | 60d → Archive |
| Dev | GitHub CI | 30d → Archive |
| *(Inbox, Gmail)* | Humans, receipts, banking, insurance, official | — |

Two source mailboxes, not one: `Gmail` holds mail forwarded from the gmail
account, so it's a second inbox and gets identical treatment — same rules,
same destination folders. Whatever the rules don't claim stays put, leaving
each mailbox working as the inbox for its own account.

Ordering is expressed as set difference, not by sequencing the moves:

- **Newsletters and Property subtracted from Promotions** — they're
  mechanically bulk too; the only difference is that you want them.
- **Orders subtracts Promotions** — order-ish words turn up in sale mail
  ("Company Gifts Ordered Before Aug 1st"), so promo is claimed first. This
  is also why the order keywords can afford to be blunt bare stems.

Stateless: every run re-derives its sets from the live mailbox. A missed run,
a killed process, or an edited rule all correct themselves on the next pass.
There is no state file to lose, which is also why `DynamicUser` is free.

## What was measured, and what it killed

Numbers from probing the live mailbox (7,711 messages), not estimates.

**There is no machine-readable "this is transactional" marker.** All three
candidates were tested and are unusable:

| Signal | Hits | Verdict |
|---|---|---|
| `schema.org` / Gmail Markup (`ParcelDelivery`) | 0–12 | Gmail-driven US convention; European shops don't implement it |
| `Auto-Submitted` (RFC 3834) | 30 | 0.4% adoption |
| `Precedence` | 207 | 2.7%, and mostly on bulk mail |

That asymmetry is the whole reason Orders is heuristic while Promotions
isn't: RFC 8058 forces bulk senders to *declare themselves*, and nothing
forces the transactional side to.

**`ORDER_SENDERS` is infrastructure, never merchants.** Carriers and
platforms are a closed set — Denmark has ~6 carriers, and Shopify fronts
thousands of shops under one identity. Enumerating individual merchants was
measured at 50 messages out of 1,370 and grows without bound, so it's out.
Unrecognised shop mail stays visible in the inbox, which is the safe
direction.

**Bare stems beat phrases.** `ordre` subsumes `din ordre` *and* repairs
`ordrebekr`, which matched literally nothing; `pakke` subsumes `din pakke`;
`order` subsumes `your order` and `order confirm`. 996 hits against 596 for
the phrase list — shorter and broader at once.

**The conservative marketing test leaks ~534 messages**, concentrated in
small retailers below the one-click mandate threshold (House of Bruar alone
was ~270). `MARKETING_SENDERS` names those rather than switching to plain
`List-Unsubscribe`, which would eventually sweep up receipts.

## Lists needing occasional maintenance

- **`MARKETING_SENDERS`** — small retailers that market without setting
  `List-Unsubscribe-Post`. The price of keeping the header test conservative.
- **`NEWSLETTERS`** — grows when you subscribe to something you mean to read.
- **`KEEP_SENDERS`** — *not* an allowlist for mail you like. It exists only
  because banks and insurers ship via the same platforms as retailers. It
  changes when you change bank, not when someone new writes to you.

## Still unverified

**The retention pass has never run.** `-n` mode never creates the folders, so
`Promotions:is_older(30)` has nothing to select and reports `NO Mailbox does
not exist`. `is_older` itself is confirmed working against Fastmail — it
returns the same count as the equivalent `arrived_before` — but the pass as a
whole is only exercised on a real run.

## Interaction with role/mail.nix

`role/mail.nix` (mbsync + mu into `~/.maildir`) is currently commented out in
`machines/saoiste`. If re-enabled, imapfilter moving mail server-side is just
normal IMAP client behaviour and mbsync handles it. `mailsort.nix` declares
`age.secrets.ftzm-org-email` with `mkDefault` so a machine importing both
roles doesn't hit a conflicting definition.
