-module(uffda_api_table).

-export([dt/0]).

dt() -> [{"/", uffda_rest_handler, []}].
