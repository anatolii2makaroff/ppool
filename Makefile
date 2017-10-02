PROJECT = ppool
PROJECT_DESCRIPTION = Simple OPT ppool
PROJECT_VERSION = 0.1.0

ERLC_OPTS= -Ddebug

include erlang.mk

run:
	exec erl \
	-sname node \
	-setcookie '123' \
	-boot start_sasl \
	-sasl errlog_type error \
	-sasl sasl_error_logger false \
	-sasl error_logger_mf_dir \"/tmp\" \
	-sasl error_logger_mf_maxbytes 10000000 \
	-sasl error_logger_mf_maxfiles 10 \
	-kernel start_pg2 true \
	-pa ./ebin ../apps/node_scheduler/ebin ../apps/node_watch/ebin \
   	-s main
