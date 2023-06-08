% @doc Helper functions for persistent database storage.
-module(braid_table).

% API
-export([init/0]).
-export([set/3]).
-export([ensure_table/2]).
-export([get/2]).
-export([find/2]).
-export([select/2]).
-export([close/0]).

-record(kv, {
    key,
    value
}).

%--- API -----------------------------------------------------------------------

init() ->
    ok = application:set_env(mnesia, dir, filename:basedir(user_data, "braid")),
    ensure_schema(),
    {ok, _} = application:ensure_all_started(mnesia).

ensure_table(Table, Opts) ->
    All = Opts ++ [{record_name, kv}, {disc_copies, [node()]}],
    case mnesia:create_table(Table, All) of
       {atomic, ok}                       -> ok;
       {aborted, {already_exists, Table}} -> ok
    end,
    ok = mnesia:wait_for_tables([Table], 5000).

set(Table, Key, Value) ->
    mnesia:dirty_write(Table, #kv{key = Key, value = Value}).

get(Table, Key) ->
    case find(Table, Key) of
        {ok, Value} -> Value;
        error       -> error({badkey, Key})
    end.

find(Table, Key) ->
    case mnesia:dirty_read(Table, Key) of
        [#kv{key = Key, value = Value}] -> {ok, Value};
        []                              -> error
    end.

select(Table, MatchSpec) ->
    mnesia:dirty_select(Table, MatchSpec).

close() ->
    mnesia:sync_log().

%--- Internal ------------------------------------------------------------------

ensure_schema() ->
    Node = node(),
    case mnesia:create_schema([Node]) of
        ok                                      -> ok;
        {error, {Node, {already_exists, Node}}} -> ok
    end.
