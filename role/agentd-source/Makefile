CC ?= cc
export PATH := $(CURDIR)/bin:$(PATH)
CPPFLAGS += -Ivendor/cjson -Isrc -D_POSIX_C_SOURCE=200809L
CFLAGS ?= -O2 -g
CFLAGS += -std=c11 -Wall -Wextra -Wpedantic
LDLIBS += -lm

.PHONY: all check check-unit check-server check-sanitize check-hooks check-claude check-live check-elisp check-replay check-ghostel check-ghostel-live clean
all: build/agent-event-server build/agentd build/agent-replay-filter

build/agent-replay-filter: src/agent-replay-filter.c
	mkdir -p build
	$(CC) $(CPPFLAGS) $(CFLAGS) $< $(LDFLAGS) -o $@

STATE_SOURCES = src/session.c src/claude.c src/codex.c src/transcript.c vendor/cjson/cJSON.c

build/agentd: src/agentd.c src/store.c src/store.h src/zmx.c src/zmx.h src/session.h $(STATE_SOURCES)
	mkdir -p build
	$(CC) $(CPPFLAGS) $(CFLAGS) src/agentd.c src/store.c src/zmx.c $(STATE_SOURCES) $(LDFLAGS) $(LDLIBS) -o $@

build/session-tests: tests/session-tests.c src/session.h $(STATE_SOURCES)
	mkdir -p build
	$(CC) $(CPPFLAGS) $(CFLAGS) -UNDEBUG tests/session-tests.c $(STATE_SOURCES) $(LDFLAGS) $(LDLIBS) -o $@

build/fixture-tests: tests/fixture-tests.c src/session.h $(STATE_SOURCES)
	mkdir -p build
	$(CC) $(CPPFLAGS) $(CFLAGS) -UNDEBUG tests/fixture-tests.c $(STATE_SOURCES) $(LDFLAGS) $(LDLIBS) -o $@

build/transcript-tests: tests/transcript-tests.c src/session.h $(STATE_SOURCES)
	mkdir -p build
	$(CC) $(CPPFLAGS) $(CFLAGS) -UNDEBUG tests/transcript-tests.c $(STATE_SOURCES) $(LDFLAGS) $(LDLIBS) -o $@

SANITIZE = -O1 -g -std=c11 -Wall -Wextra -Wpedantic -fsanitize=address,undefined -fno-omit-frame-pointer

build/agent-replay-filter-sanitized: src/agent-replay-filter.c
	mkdir -p build
	$(CC) $(CPPFLAGS) $(SANITIZE) $< $(LDFLAGS) -o $@

build/agentd-sanitized: src/agentd.c src/store.c src/store.h src/zmx.c src/zmx.h src/session.h $(STATE_SOURCES)
	mkdir -p build
	$(CC) $(CPPFLAGS) $(SANITIZE) src/agentd.c src/store.c src/zmx.c $(STATE_SOURCES) $(LDFLAGS) $(LDLIBS) -o $@

build/session-tests-sanitized: tests/session-tests.c src/session.h $(STATE_SOURCES)
	mkdir -p build
	$(CC) $(CPPFLAGS) $(SANITIZE) -UNDEBUG tests/session-tests.c $(STATE_SOURCES) $(LDFLAGS) $(LDLIBS) -o $@

build/fixture-tests-sanitized: tests/fixture-tests.c src/session.h $(STATE_SOURCES)
	mkdir -p build
	$(CC) $(CPPFLAGS) $(SANITIZE) -UNDEBUG tests/fixture-tests.c $(STATE_SOURCES) $(LDFLAGS) $(LDLIBS) -o $@

build/transcript-tests-sanitized: tests/transcript-tests.c src/session.h $(STATE_SOURCES)
	mkdir -p build
	$(CC) $(CPPFLAGS) $(SANITIZE) -UNDEBUG tests/transcript-tests.c $(STATE_SOURCES) $(LDFLAGS) $(LDLIBS) -o $@

build/agent-event-server: src/agent-event-server.c vendor/cjson/cJSON.c vendor/cjson/cJSON.h
	mkdir -p build
	$(CC) $(CPPFLAGS) $(CFLAGS) src/agent-event-server.c vendor/cjson/cJSON.c $(LDFLAGS) $(LDLIBS) -o $@

build/harness-http: tests/harness-http.c vendor/cjson/cJSON.c
	mkdir -p build
	$(CC) $(CPPFLAGS) $(CFLAGS) tests/harness-http.c vendor/cjson/cJSON.c $(LDFLAGS) -o $@

check: build/agent-event-server check-unit check-server check-replay
	bash tests/agent-event-tests.sh
	bash tests/hook-definitions-tests.sh

check-unit: build/session-tests build/fixture-tests build/transcript-tests
	./build/session-tests
	./build/fixture-tests
	./build/transcript-tests

check-server: build/agentd build/agent-replay-filter
	bats tests/server.bats tests/lifecycle.bats tests/zmx-live.bats tests/restart.bats

check-sanitize: build/agentd-sanitized build/session-tests-sanitized build/fixture-tests-sanitized build/transcript-tests-sanitized build/agent-replay-filter-sanitized build/agent-replay-filter
	ASAN_OPTIONS=detect_leaks=1:abort_on_error=1 UBSAN_OPTIONS=halt_on_error=1 AGENT_REPLAY_FILTER=$(CURDIR)/build/agent-replay-filter-sanitized bash tests/replay-filter-tests.sh
	ASAN_OPTIONS=detect_leaks=1:abort_on_error=1 UBSAN_OPTIONS=halt_on_error=1 ./build/session-tests-sanitized
	ASAN_OPTIONS=detect_leaks=1:abort_on_error=1 UBSAN_OPTIONS=halt_on_error=1 ./build/fixture-tests-sanitized
	ASAN_OPTIONS=detect_leaks=1:abort_on_error=1 UBSAN_OPTIONS=halt_on_error=1 ./build/transcript-tests-sanitized
	ASAN_OPTIONS=detect_leaks=1:abort_on_error=1 UBSAN_OPTIONS=halt_on_error=1 AGENTD_TEST_BINARY=$(CURDIR)/build/agentd-sanitized bats tests/server.bats tests/lifecycle.bats tests/zmx-live.bats tests/restart.bats

.PHONY: check-claude-resume
check-claude-resume: build/agentd build/harness-http build/agent-replay-filter
	REDRAW_MODES=resume REDRAW_TABLE=1 CLAUDE_CODE_DISABLE_ALTERNATE_SCREEN=1 bash tests/investigation/claude-redraw.sh

check-hooks: build/agent-event-server
	bash tests/hook-definitions-tests.sh

check-replay: build/agent-replay-filter
	bash tests/replay-filter-tests.sh

check-claude: build/agent-event-server
	bash tests/claude-hook-smoke.sh

check-live: build/agentd build/harness-http build/mcp-fixture build/agent-replay-filter
	bash tests/harness-live.sh

check-elisp: build/agentd build/agent-replay-filter
	emacs --batch -Q --eval '(setq load-prefer-newer t)' -L . -l tests/agentd-client-tests.el -l tests/agentd-terminal-tests.el -l tests/agentd-status-tests.el -l tests/agentd-attention-tests.el -l tests/agentd-perspective-tests.el -l tests/agentd-completion-tests.el -l tests/agentd-package-tests.el -l tests/agentd-recovery-tests.el -l tests/agentd-launch-tests.el -l tests/agentd-session-tests.el -f ert-run-tests-batch-and-exit
	bats tests/emacs-client.bats

check-ghostel: build/agentd build/agent-replay-filter
	bash tests/emacs-spike.sh

check-ghostel-live: build/agentd build/harness-http build/mcp-fixture build/agent-replay-filter
	AGENT_VERIFY_GHOSTEL=1 bash tests/harness-live.sh

clean:
	rm -f build/agent-replay-filter
	rm -f build/agent-replay-filter-sanitized
	rm -f build/agent-event-server build/agentd build/session-tests
	rm -f build/agentd-sanitized build/session-tests-sanitized
	rm -f build/fixture-tests build/fixture-tests-sanitized build/harness-http
	rm -f build/transcript-tests build/transcript-tests-sanitized

build/mcp-fixture: tests/mcp-fixture.c vendor/cjson/cJSON.c
	mkdir -p build
	$(CC) $(CPPFLAGS) $(CFLAGS) $^ $(LDFLAGS) -o $@
