-module(braid_rest).

-export([launch/1]).
-export([list/1]).
-export([destroy/1]).

% API --------------------------------------------------------------------------

launch(ConfigPath) ->
    Config = parse_config(ConfigPath),
    Hosts = parse_hosts(Config),
    [{H, send_launch_config(H, Config)} || H <- Hosts].

list(Host) -> send_list_req(Host).

destroy(ConfigPath) ->
    Config = parse_config(ConfigPath),
    Hosts = parse_hosts(Config),
    [{H, send_destroy_config(H, Config)} || H <- Hosts].

% internal ---------------------------------------------------------------------

send_launch_config(Host, Config) ->
    URI = compose_uri(Host, "launch"),
    ContentType = "application/json",
    Body = jsx:encode(Config),
    io:format("sending :~p~n",[Body]),
    {ok, Result} = httpc:request(post, {URI, [], ContentType, Body},
                                 [],
                                 [{body_format, binary}]),
    {{_HTTP, 200, "OK"}, _Headers, Reply} = Result,
    json_decode(Reply).

send_destroy_config(Host, Config) ->
    URI = compose_uri(Host, "destroy"),
    ContentType = "application/json",
    Body = jsx:encode(Config),
    io:format("sending :~p~n",[Body]),
    {ok, Result} = httpc:request(delete, {URI, [], ContentType, Body},
                                 [],
                                 [{body_format, binary}]),
    {{_HTTP, 200, "OK"}, _Headers, Reply} = Result,
    json_decode(Reply).

send_list_req(Host) ->
    URI = compose_uri(Host, "list"),
    {ok, Result} = httpc:request(get, {URI, []}, [], [{body_format, binary}]),
    {{_HTTP, 200, "OK"}, _Headers, Body} = Result,
    json_decode(Body).

parse_config(ConfigPath) ->
    {ok, [Config]} = file:consult(ConfigPath),
    io:format("Using config: ~p~n", [Config]),
    Config.

parse_hosts(Config) ->
    [ begin
        [_, Hostname] = string:split(atom_to_list(Orchestrator), "@"),
        Hostname
    end || {Orchestrator, _Nodes} <- maps:to_list(Config)].

compose_uri(Host, Method) ->
    "http://" ++ Host ++ ":8080/api/" ++ Method.

json_decode(JSON) ->
    jsx:decode(JSON, [{return_maps, true},{labels, binary}]).
