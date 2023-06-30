-module(braid_app).

-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
    braid_table:init(),
    braid_net:init(),
    {ok, self()}.

stop(_State) -> ok.

%% internal functions
