PROJECT = ppool
PROJECT_DESCRIPTION = Simple OPT ppool
PROJECT_VERSION = 0.1.0

ERLC_OPTS= -Ddebug1

include erlang.mk

run:
	./scripts/drop start
