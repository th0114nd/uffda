PROJECT = uffda
DEPS = lager erlang_commons
dep_lager = https://github.com/basho/lager
dep_erlang_commons = https://github.com/jaynel/erlang_commons

SERVER := erl -pa ebin -pa deps/*/ebin -smp enable -s lager -setcookie CISFORCOOKIE
HOST := `hostname` 

.PHONY: release clean-release

run: all
	if [ -n "${NODE}" ]; then ${SERVER} -name ${NODE}@${HOST} -boot start_sasl -s uffda; \
	else ${SERVER} -name uffda@${HOST} -boot start_sasl -s uffda; \
	fi

relxrun: release
	rel/uffda/uffda/bin/uffda console

release: clean-release all
	relx -o rel/$(PROJECT)

clean-release: clean-all
	rm -rf rel/$(PROJECT)

include erlang.mk
