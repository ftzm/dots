-- Runs role/mailsort.lua against a stub IMAP account, so the retry around a
-- transient Fastmail failure is exercised without a network or a credential.
--
--   lua role/tests/mailsort.test.lua [path/to/mailsort.lua]
--
-- The stub answers the whole surface the script uses -- contain_field,
-- contain_subject, is_older, move_messages, and the set algebra over them --
-- and raises on the first operation of a pass while failures remain, which is
-- what a refused login looks like from inside the script.

local script = arg[1] or 'role/mailsort.lua'

local function build_env(fails_remaining)
    local trace = { sleeps = {}, moves = 0, attempts = 0 }

    local set = {}
    set.__index = set
    local function new_set() return setmetatable({}, set) end
    set.__add = new_set
    set.__sub = new_set
    set.__mul = new_set
    function set:move_messages(_) trace.moves = trace.moves + 1 end

    local mailbox = {}
    mailbox.__index = mailbox
    function mailbox:contain_field(_, _)
        -- First operation of a pass: fail here while failures remain, which
        -- aborts the pass exactly as a login rejection does.
        if fails_remaining > 0 then
            fails_remaining = fails_remaining - 1
            trace.attempts = trace.attempts + 1
            error('stub: login request failed', 0)
        end
        if not self.counted then
            self.counted = true
            trace.attempts = trace.attempts + 1
        end
        return new_set()
    end
    function mailbox:contain_subject(_) return new_set() end
    function mailbox:is_older(_) return new_set() end
    function mailbox:move_messages(_) trace.moves = trace.moves + 1 end

    local account = setmetatable({}, {
        __index = function(t, _)
            local m = setmetatable({}, mailbox)
            return m
        end,
    })

    local env = {
        options = {},
        IMAP = function(_) return account end,
        sleep = function(seconds) table.insert(trace.sleeps, seconds) end,
        print = function(_) end,
        os = { getenv = function(name) return name == 'HOME' and '/nonexistent' or nil end },
        io = {
            open = function(_, _)
                return {
                    read = function() return 'stub-password' end,
                    close = function() end,
                }
            end,
        },
    }
    setmetatable(env, { __index = _G })
    return env, trace
end

local function run(fails)
    local env, trace = build_env(fails)
    local chunk, err = loadfile(script, 't', env)
    if not chunk then error('cannot load ' .. script .. ': ' .. tostring(err)) end
    local ok, run_err = pcall(chunk)
    return ok, run_err, trace
end

local failures = 0
local function check(name, cond, detail)
    if cond then
        print('ok   - ' .. name)
    else
        failures = failures + 1
        print('FAIL - ' .. name .. (detail and (': ' .. tostring(detail)) or ''))
    end
end

local function joined(t)
    local parts = {}
    for _, v in ipairs(t) do parts[#parts + 1] = tostring(v) end
    return '[' .. table.concat(parts, ',') .. ']'
end

-- A clean run sorts mail and never sleeps.
local ok, err, trace = run(0)
check('clean run succeeds', ok, err)
check('clean run moves messages', trace.moves > 0, trace.moves)
check('clean run does not sleep', #trace.sleeps == 0, joined(trace.sleeps))

-- Two failed passes are a blip: the run recovers, having backed off between
-- attempts, and still does the work.
ok, err, trace = run(2)
check('transient failure recovers', ok, err)
check('transient failure backs off 20s then 40s',
      #trace.sleeps == 2 and trace.sleeps[1] == 20 and trace.sleeps[2] == 40,
      joined(trace.sleeps))
check('transient failure still moves messages', trace.moves > 0, trace.moves)

-- Permanent breakage exhausts the attempts and must fail the unit, or a
-- rejected password would look like a healthy run forever.
ok, err, trace = run(99)
check('permanent failure raises', not ok, 'run unexpectedly succeeded')
check('permanent failure reports the underlying error',
      type(err) == 'string' and err:find('login request failed', 1, true) ~= nil, err)
check('permanent failure tries four times and sleeps three times',
      #trace.sleeps == 3 and trace.sleeps[3] == 80, joined(trace.sleeps))

if failures > 0 then
    print(failures .. ' failed')
    os.exit(1)
end
print('all passed')
