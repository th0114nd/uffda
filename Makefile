PROJECT = uffda

DEPS = cowboy eper

TEST_DEPS = proper test_commons
dep_test_commons = git https://github.com/th0114nd/test_commons master

# Needed for testing
CT_OPTS := -cover test/uffda.coverspec
CT_SUITES := uffda_registry uffda_service uffda_system

DIALYZER_OPTS := test/uffda -Werror_handling -Wrace_conditions -Wunmatched_returns
EDOC_DIRS := ["src", "examples", "test/uffda", "test/test_commons"]
EDOC_OPTS := {preprocess, true}, {source_path, ${EDOC_DIRS}}, nopackages, {subpackages, true}

DEV_SERVER := erl -pa test -pa deps/*/ebin -pa ../uffda/ebin -smp enable -setcookie CISFORCOOKIE
RUN_SERVER := erl -pa deps/*/ebin -pa ../uffda/ebin -smp enable -setcookie CISFORCOOKIE
HOST := `hostname` 

include erlang.mk
run: all patch
	if [ -n "${NODE}" ]; then ${RUN_SERVER} -boot start_sasl -s uffda; \
	else ${RUN_SERVER} -boot start_sasl -s uffda; \
	fi

dev: all #build-ct-suites patch
	if [ -n "${NODE}" ]; then ${DEV_SERVER} -boot start_sasl; \
	else ${DEV_SERVER} -boot start_sasl; \
	fi

relxrun: release
	rel/uffda/uffda/bin/uffda console

images: doc
	mkdir -p doc/images
	dot -Tpng doc/states.dot -o doc/images/states.png
	dot -Tpng doc/tc_proper_model_behaviour.dot -o doc/images/tc_proper_model_behaviour.png
	dot -Tpng doc/unify.dot -o doc/images/unify.png
	dot -Tpng doc/test_hierarchy.dot -o doc/images/test_hierarchy.png

release: clean-release all
	relx -o rel/$(PROJECT)

#.IGNORE: patch
#patch: deps
#	patch -d deps/proper -N -p1 < 0001-Also-accept-native-types-in-LETs.patch
#   
#unpatch: deps
#	patch -d deps/proper -R -p1 < 0001-Also-accept-native-types-in-LETs.patch

test: tests

clean::
	rm -rf rel/$(PROJECT)
