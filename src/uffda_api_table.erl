-module(uffda_api_table).

-export([dt/0]).

dt() -> [{"/", rest_handler, []}].
