PROJECT = uffda

TEST_DEPS = proper
dep_proper = https://github.com/manopapad/proper v1.1

# Needed for testing
CT_OPTS := -cover test/uffda.coverspec
CT_SUITES := uffda_registry

EDOC_OPTS := {preprocess, true}, {source_path, ["src", "examples", "test/uffda"]}, nopackages, {subpackages, true}

SERVER := erl -pa ebin -pa deps/*/ebin -smp enable -setcookie CISFORCOOKIE
HOST := `hostname` 


.PHONY: release clean-release

run: all
	if [ -n "${NODE}" ]; then ${SERVER} -name ${NODE}@${HOST} -boot start_sasl -s uffda; \
	else ${SERVER} -name uffda@${HOST} -boot start_sasl -s uffda; \
	fi

relxrun: release
	rel/uffda/uffda/bin/uffda console

images: doc
	mkdir -p doc/images
	dot -Tpng doc/states.dot -o doc/images/states.png

release: clean-release all
	relx -o rel/$(PROJECT)

clean-release: clean-all
	rm -rf rel/$(PROJECT)

tags: all
	/opt/local/bin/ctags --file-scope=no -R --languages=erlang .

include erlang.mk
