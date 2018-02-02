PROJECT = ppool
PROJECT_DESCRIPTION = Simple OPT ppool
PROJECT_VERSION = 0.1.0
DEPS = cowboy
# dep_cowboy_commit = 2.2.2

ERLC_OPTS= -Ddebug0

include erlang.mk

run:
	./scripts/drop start
