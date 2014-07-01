PROJECT = uffda
DEPS = lager erlang_commons
SERVER := erl -pa ebin -pa deps/*/ebin -smp enable -s lager -setcookie CISFORCOOKIE
HOST := `hostname` 

run: all
	if [ -n "${NODE}" ]; then ${SERVER} -name ${NODE}@${HOST} -boot start_sasl -s uffda; \
	else ${SERVER} -name uffda@${HOST} -boot start_sasl -s uffda; \
	fi

dep_lager = https://github.com/basho/lager
dep_erlang_commons = https://github.com/jaynel/erlang_commons
include erlang.mk
