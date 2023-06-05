-module(braid_rest).

-export([launch/1]).
-export([list/1]).
-export([destroy/1]).

% API --------------------------------------------------------------------------

launch(ConfigPath) ->
    Config = parse_config(ConfigPath),
    Orchestrators = parse_instances(Config),
    [{Orch, send_to_instance(post, Orch, "launch", Config)} ||
        Orch <- Orchestrators].

list(ConfigPath) ->
    Config = parse_config(ConfigPath),
    Orchestrators = parse_instances(Config),
    [{Orch, send_to_instance(get, Orch, "list", [])} ||
        Orch <- Orchestrators].

destroy(ConfigPath) ->
    Config = parse_config(ConfigPath),
    Orchestrators = parse_instances(Config),
    [{Orch, send_to_instance(delete, Orch, "destroy", Config)} ||
        Orch <- Orchestrators].

% internal ---------------------------------------------------------------------

send_to_instance(Method, Instance, API, Message) ->
    URI = compose_uri(API),
    H = headers(Instance),
    Request = case Method of
        get -> {URI, H};
        _ ->
            ContentType = "application/json",
            Body = jsx:encode(Message),
            io:format("sending :~p~n",[Body]),
            {URI, H, ContentType, Body}
    end,
    {ok, Result} = httpc:request(Method, Request,
                                 http_opts(),
                                 [{body_format, binary}]),
    {{_HTTP, 200, "OK"}, _Headers, Reply} = Result,
    json_decode(Reply).

parse_config(ConfigPath) ->
    {ok, [Config]} = file:consult(ConfigPath),
    io:format("Using config: ~p~n", [Config]),
    Config.

parse_instances(Config) ->
    [ begin
        atom_to_binary(Orchestrator)
    end || {Orchestrator, _Nodes} <- maps:to_list(Config)].

compose_uri(Method) ->
    {ok, Scheme} = application:get_env(braid, scheme),
    {ok, Domain} = application:get_env(braid, domain),
    {ok, Port} = application:get_env(braid, port),
    uri_string:recompose(#{
        scheme => Scheme,
        host => Domain,
        port => Port,
        path => "/api/" ++ Method
    }).

json_decode(JSON) ->
    jsx:decode(JSON, [{return_maps, true},{labels, binary}]).

headers(Instance) ->
    {ok, Token} = application:get_env(braid, token),
    [
        {"Authorization", "Bearer " ++ Token},
        {"fly-force-instance-id", Instance}
    ].


http_opts() ->
    {ok, Scheme} = application:get_env(braid, scheme),
    case Scheme of
        "https" ->
            [
                {ssl, [
                    {verify, verify_peer},
                    {cacerts, certifi:cacerts()},
                    {customize_hostname_check, [
                        {match_fun,
                            public_key:pkix_verify_hostname_match_fun(https)}
                    ]}
                ]}
            ];
        _ -> []
     end.
