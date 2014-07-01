PROJECT = uffda

DEPS = erlang_commons proper
dep_erlang_commons = https://github.com/jaynel/erlang_commons
dep_proper = https://github.com/manopapad/proper

# Needed for testing
CT_OPTS = -cover test/uffda.coverspec
CT_SUITES = uffda_basic

# Specifies where to look when building the plt
ALL_DEPS_DIRS = --output_plt ./.uffda.plt -pa deps/*/ebin ebin

SERVER := erl -pa ebin -pa deps/*/ebin -smp enable -s lager -setcookie CISFORCOOKIE
HOST := `hostname` 


.PHONY: release clean-release

run: all
	if [ -n "${NODE}" ]; then ${SERVER} -name ${NODE}@${HOST} -boot start_sasl -s uffda; \
	else ${SERVER} -name uffda@${HOST} -boot start_sasl -s uffda; \
	fi

real_build_plt: app
	dialyzer --verbose --build_plt --apps erts kernel stdlib \
        --output_plt ./.uffda.plt -pa deps/*/ebin ebin
relxrun: release
	rel/uffda/uffda/bin/uffda console

release: clean-release all
	relx -o rel/$(PROJECT)

clean-release: clean-all
	rm -rf rel/$(PROJECT)

include erlang.mk
