{alias, uffda, "./uffda/"}.
{include, ["../include"]}.
{cover, "./uffda.coverspec"}.
{suites, uffda, [
                  uffda_registry_SUITE.erl
                ]}.
