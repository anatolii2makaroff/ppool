PROJECT = ppool
PROJECT_DESCRIPTION = Simple OPT ppool
PROJECT_VERSION = 0.1.0

ERLC_OPTS= -Ddebug

include erlang.mk

run:
	make && erl -pa ./ebin -s main
