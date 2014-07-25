PROJECT = uffda

DEPS = cowboy eper
dep_cowboy = pkg://cowboy 0.9.0
dep_eper =  pkg://eper 0.85.0

TEST_DEPS = proper test_commons
dep_proper = pkg://proper v1.1
dep_test_commons = https://github.com/th0114nd/test_commons/


# Needed for testing
CT_OPTS := -cover test/uffda.coverspec
CT_SUITES := uffda_registry uffda_service uffda_system

PLT_APPS := compiler crypto
DIALYZER_OPTS := test/uffda -Werror_handling -Wrace_conditions -Wunmatched_returns
EDOC_DIRS := ["src", "examples", "test/uffda", "test/test_commons"]
EDOC_OPTS := {preprocess, true}, {source_path, ${EDOC_DIRS}}, nopackages, {subpackages, true}

#TEST_ERLC_OPTS := -pa test -pa deps/*/ebin -pa ebin
DEV_SERVER := erl -pa test -pa deps/*/ebin -pa ../uffda/ebin -smp enable -setcookie CISFORCOOKIE
RUN_SERVER := erl -pa deps/*/ebin -pa ../uffda/ebin -smp enable -setcookie CISFORCOOKIE
HOST := `hostname` 


.PHONY: release clean-release patch

run: all patch
	if [ -n "${NODE}" ]; then ${RUN_SERVER} -boot start_sasl -s uffda; \
	else ${RUN_SERVER} -boot start_sasl -s uffda; \
	fi

dev: all build-tests patch
	if [ -n "${NODE}" ]; then ${DEV_SERVER} -boot start_sasl; \
	else ${DEV_SERVER} -boot start_sasl; \
	fi

relxrun: release
	rel/uffda/uffda/bin/uffda console

images: doc
	mkdir -p doc/images
	dot -Tpng doc/states.dot -o doc/images/states.png
	dot -Tpng doc/tcb_model.dot -o doc/images/tcb_model.png
	dot -Tpng doc/unify.dot -o doc/images/unify.png
	dot -Tpng doc/test_hierarchy.dot -o doc/images/test_hierarchy.png

release: clean-release all
	relx -o rel/$(PROJECT)

.IGNORE: patch
patch: deps
	patch -d deps/proper -N -p1 < 0001-Also-accept-native-types-in-LETs.patch
   
unpatch: deps
	patch -d deps/proper -R -p1 < 0001-Also-accept-native-types-in-LETs.patch

clean-release: clean-all
	rm -rf rel/$(PROJECT)

tags: all
	/opt/local/bin/ctags --file-scope=no -R --languages=erlang .

include erlang.mk
