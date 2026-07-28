-- Fastmail sorting and retention, for imapfilter.
--
-- Sieve can't do this job: Fastmail exposes no API to install a script (no
-- ManageSieve, and urn:ietf:params:jmap:sieve is unimplemented on their JMAP
-- endpoint), and Sieve runs at delivery so it has no concept of a message's
-- age -- which the retention pass below depends on.
--
-- Stateless. Every run re-derives the sets from the live mailbox, so a missed
-- run, a killed process, or an edited rule all correct themselves next pass.
--
--   imapfilter -n -c mailsort.lua    dry run: says what it would do, moves nothing
--   imapfilter -c mailsort.lua       do it

options.timeout = 60
options.namespace = true   -- read the server's prefix/delimiter; we write '/'
options.create = true      -- create destination folders on first move
options.subscribe = true   -- ...and subscribe them, so the webmail UI shows them
options.info = true        -- per-run summary; this is the move log in journald

-- Credential -------------------------------------------------------------
-- systemd LoadCredential puts it in $CREDENTIALS_DIRECTORY. Falls back to a
-- path under $HOME so the config can be run by hand for dry runs.

local function credential(name)
    local dir = os.getenv('CREDENTIALS_DIRECTORY')
    local home = os.getenv('HOME')
    local path
    if dir then
        path = dir .. '/' .. name
    elseif home then
        path = home .. '/.config/mailsort/' .. name
    else
        error('no CREDENTIALS_DIRECTORY and no HOME; nowhere to read ' .. name)
    end
    local fh = io.open(path, 'r')
    if not fh then
        error('cannot read credential: ' .. path)
    end
    local secret = fh:read('*l')
    fh:close()
    return secret
end

-- Rules ------------------------------------------------------------------
-- These are matched as substrings of the From header, so a bare domain and a
-- full address are both fine.

local DEV = { 'github.com' }

-- 'substack.com' is a deliberate wildcard, not an oversight: Substack is
-- essentially never a promotional origin, so catching every subscription is
-- the right default and needs no edit when you subscribe to a new one. The
-- other entries are specific addresses because their domains aren't safe to
-- wildcard that way.
local NEWSLETTERS = {
    'sampsonboatco@creator.patreon.com',
    'nyhedsbrev@fbr.dk',
    'info@haskellweekly.news',
    'substack.com',
}

-- Institutional senders pinned to the inbox. Not a list of mail you like --
-- it exists only because banks and insurers ship via the same platforms as
-- retailers, so the header test alone can't be trusted for them. Bounded:
-- changes when you change bank, not when someone new writes to you.
-- Note the Gjensidige split -- documents come from gjensidige.dk, marketing
-- from info.gjensidige.dk, so only the first is listed and the second falls
-- through to Promotions on its own.
local KEEP_SENDERS = {
    'No_replymail@gjensidige.dk',
    'noreply@wise.com',
    -- rejsekort is deliberately absent: it sends 171 travel receipts, which
    -- RECEIPT_SUBJECTS files properly. Anything it sends that isn't a receipt
    -- has no receipt word in the subject and falls through to the inbox on
    -- its own, so pinning the sender bought nothing and blocked the folder.
    'noreply@ucplus.dk',
    'klinik@tandnini.dk',
    'nordaccount.com',
    'borgbase.com',
    'namecheap.com',
    'notify.cloudflare.com',
    'account-noreply@ui.com',
    'mail.anthropic.com',
}

-- Auth and account security. Kept out of Promotions because some providers
-- (OpenAI) send login codes and product marketing from the same address.
local AUTH_SUBJECTS = {
    'verification code', 'verify your', 'sign in', 'sign-in', 'log ind',
    'login', 'password', 'authentication', 'two-factor',
    'security alert', 'authorization code', 'secure link',
    'new device', 'unusual activity',
}

-- Receipts. Deliberately matched on subject, never on sender: sampling
-- showed that four of the five biggest "receipt" senders also send genuinely
-- actionable mail from the same address. Filing by sender would have hidden
-- a past-due invoice from Anthropic, a new-device login alert from PayPal, a
-- card-expiry warning from Greentel that would have killed a phone
-- subscription, and commuter travel days with a deadline from DSB.
--
-- These words separate cleanly because the actionable items avoid them
-- entirely: "Reminder: Your *invoice* ... past due" is not a receipt,
-- "Login from a new device" is not a kvittering.
local RECEIPT_SUBJECTS = {
    'receipt', 'kvittering', 'optankning',
}

-- Property alerts. Boligsiden saved-search hits alone were ~420 messages,
-- the single largest category left in the inbox before this existed.
local PROPERTY = {
    'boligsiden.dk',
    'cvbbolig.dk',
    'myhome.home.dk',
}

-- Retailers that market without setting List-Unsubscribe-Post. The RFC 8058
-- one-click header is only mandated for high-volume senders, so small shops
-- fall through the test below. Found by probing the inbox residue, not
-- guessed. Adding a name here is the maintenance cost of keeping the header
-- test conservative -- the alternative was matching plain List-Unsubscribe
-- and eventually sweeping up receipts.
local MARKETING_SENDERS = {
    'houseofbruar.com',
    'nordvpn.com',        -- billing comes from nordaccount.com, in KEEP above
    'risteriet.dk',
    'keeb.io',
    'walkerslater.com',
    'lampemesteren.dk',
    'komoot.de',
    'comms.trainline.com',
}

-- The carriers were originally drawn from a 180-message sample that happened
-- Infrastructure only: carriers, e-commerce platforms, review platforms.
-- Deliberately NOT individual shops. There are about six parcel carriers in
-- Denmark and Shopify fronts thousands of merchants under one identity, so
-- this set is closed and stops growing. Enumerating merchants instead was
-- measured at 50 messages out of 1370 -- not worth an unbounded list, and it
-- fails safe: an unrecognised shop's order mail stays visible in the inbox.
local ORDER_SENDERS = {
    'bring.com', 'gls-group.eu', 'gls-denmark.com', 'postnord.dk',
    'postnord.com', 'dhl.com', 'pakkeshop.dk', 'shopify.com',
    'trustpilotmail.com',
}

-- Bare stems, not phrases: 'ordre' subsumes 'din ordre' and 'ordrebekr'
-- (which matched nothing at all), 'pakke' subsumes 'din pakke', 'order'
-- subsumes 'your order' and 'order confirm'. Measured at 996 hits against
-- 596 for the phrase list -- shorter and broader at once.
--
-- These can afford to be blunt because Orders is computed after Promotions
-- is subtracted, so marketing can never reach them.
--
-- All ASCII on purpose: IMAP SEARCH with non-ASCII needs a CHARSET dance and
-- is historically flaky. That is also why 'bekraeftelse' is absent -- it
-- scored zero, since the real word is spelt with an ae ligature.
--
-- Financial documents (receipt, kvittering, faktura, invoice -- ~350
-- messages) are deliberately excluded: those stay in the inbox.
local ORDER_SUBJECTS = {
    'ordre', 'pakke', 'bestilling', 'afsendt', 'levering', 'leveret',
    'forsendelse', 'order', 'shipment', 'shipped', 'dispatched',
    'delivery', 'tracking', 'on its way',
}

-- Folder -> days before it is swept to Archive. Ordered, because Lua tables
-- with string keys have no defined iteration order and the log should read
-- the same way every run.
--
-- Newsletters gets a longer window than the rest: it holds things you mean
-- to get back to, not noise to be got out of the way.
local AGE_OUT = {
    { 'Promotions', 30 },
    { 'Orders', 30 },
    { 'Dev', 30 },
    { 'Property', 30 },
    { 'Newsletters', 60 },
    -- Records rather than noise: a year keeps the current tax year to hand.
    -- Nothing is lost either way, Archive stays searchable.
    { 'Receipts', 365 },
}

-- Sender -> days, for mail that expires *in place* rather than being filed.
-- It stays in the inbox where you will see it, then goes to Archive once
-- it is stale.
--
-- Digital Post notifications only tell you that something is waiting on the
-- portal; the content is never in the mail itself. Once you have been told,
-- the message has no further value -- but filing it on arrival would defeat
-- the point of being told.
local EXPIRE_IN_PLACE = {
    { 'digitalpost.dk', 7 },
}

-- Helpers ----------------------------------------------------------------

local function union(sets)
    local acc = sets[1]
    for i = 2, #sets do
        acc = acc + sets[i]
    end
    return acc
end

local function any_from(mbox, values)
    local sets = {}
    for _, v in ipairs(values) do
        table.insert(sets, mbox:contain_field('from', v))
    end
    return union(sets)
end

local function any_subject(mbox, words)
    local sets = {}
    for _, w in ipairs(words) do
        table.insert(sets, mbox:contain_subject(w))
    end
    return union(sets)
end

-- Account ----------------------------------------------------------------

local account = IMAP {
    server = 'imap.fastmail.com',
    username = 'm@ftzm.org',
    password = credential('imap-password'),
    ssl = 'auto',
}

-- Both of these are inboxes. 'Gmail' holds mail forwarded from the gmail
-- account, so it gets identical treatment -- same rules, same destination
-- folders. Whatever the rules don't claim stays put, which leaves each one
-- working as the inbox for its own account.
local SOURCES = { 'INBOX', 'Gmail' }

-- Sorting ----------------------------------------------------------------
-- Every set is computed before anything moves, so the ordering below is
-- expressed as set difference rather than by sequencing the moves.

local function sort_mailbox(name)
    local mbox = account[name]

    local keep = any_from(mbox, KEEP_SENDERS)
               + any_subject(mbox, AUTH_SUBJECTS)

    local dev = any_from(mbox, DEV) - keep

    local news = any_from(mbox, NEWSLETTERS) - keep

    local property = any_from(mbox, PROPERTY) - keep

    -- RFC 8058 one-click unsubscribe. Bulk senders are required to set it
    -- and transactional mail is exempt, which is a far sharper line than
    -- plain List-Unsubscribe (receipts carry that too). Per RFC 3501 a
    -- zero-length search string matches any message that merely *has* the
    -- header.
    --
    -- MARKETING_SENDERS covers the small retailers that market without
    -- setting it, since the one-click mandate only binds high-volume senders.
    local promo = (mbox:contain_field('list-unsubscribe-post', '')
                 + any_from(mbox, MARKETING_SENDERS))
                - news - dev - property - keep

    -- Before Orders: the receipt words are the more specific of the two, and
    -- an order confirmation that also calls itself a kvittering is a record
    -- rather than a lifecycle notification.
    local receipts = any_subject(mbox, RECEIPT_SUBJECTS)
                   - promo - news - dev - property - keep

    -- After the marketing test on purpose: order-ish words turn up in sale
    -- mail ("Company Gifts Ordered Before Aug 1st"), and by this point
    -- anything promotional has already been claimed above.
    local orders = (any_from(mbox, ORDER_SENDERS)
                  + any_subject(mbox, ORDER_SUBJECTS))
                 - receipts - promo - news - dev - property - keep

    dev:move_messages(account['Dev'])
    news:move_messages(account['Newsletters'])
    property:move_messages(account['Property'])
    promo:move_messages(account['Promotions'])
    receipts:move_messages(account['Receipts'])
    orders:move_messages(account['Orders'])
end

for _, name in ipairs(SOURCES) do
    sort_mailbox(name)
end

-- Anything with no bulk markers at all -- humans, receipts, official mail --
-- is never selected here and simply stays where it was. Reaching you is the
-- default path, not a listed exception.

-- Retention --------------------------------------------------------------

for _, rule in ipairs(EXPIRE_IN_PLACE) do
    local sender, days = rule[1], rule[2]
    for _, src in ipairs(SOURCES) do
        local mbox = account[src]
        local stale = mbox:contain_field('from', sender) * mbox:is_older(days)
        stale:move_messages(account['Archive'])
    end
end

for _, rule in ipairs(AGE_OUT) do
    local name, days = rule[1], rule[2]
    account[name]:is_older(days)
                 :move_messages(account['Archive'])
end
