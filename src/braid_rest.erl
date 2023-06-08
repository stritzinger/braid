-module(braid_rest).

-export([launch/1]).
-export([list/1]).
-export([logs/2]).
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

logs(Instance, CID) ->
    send_to_instance(get, Instance, "logs", [{"cid", CID}]).

destroy(ConfigPath) ->
    Config = parse_config(ConfigPath),
    Orchestrators = parse_instances(Config),
    [{Orch, send_to_instance(delete, Orch, "destroy", Config)} ||
        Orch <- Orchestrators].

% internal ---------------------------------------------------------------------

send_to_instance(Method, Instance, API, Message) ->
    URIMap = compose_uri_map(API),
    H = headers(Instance),
    Request = case Method of
        get ->
            Query = uri_string:compose_query(Message),
            URI = uri_string:recompose(URIMap#{query => Query}),
            {URI, H};
        _ ->
            ContentType = "application/json",
            Body = jsx:encode(Message),
            {uri_string:recompose(URIMap), H, ContentType, Body}
    end,
    % io:format("sending ~p\n",[Request]),
    {ok, Result} = httpc:request(Method, Request,
                                 http_opts(),
                                 [{body_format, binary}]),
    {{_HTTP, Code, Status}, _Headers, Reply} = Result,
    case Reply of
        <<>> -> {Code, Status};
        _ -> {Code, json_decode(Reply)}
    end.


parse_config(ConfigPath) ->
    {ok, [Config]} = file:consult(ConfigPath),
    Config.

parse_instances(Config) ->
    [ begin
        atom_to_binary(Orchestrator)
    end || {Orchestrator, _Nodes} <- maps:to_list(Config)].

compose_uri_map(Method) ->
    {ok, Scheme} = application:get_env(braid, scheme),
    {ok, Domain} = application:get_env(braid, braidnet_domain),
    {ok, Port} = application:get_env(braid, port),
    #{
        scheme => Scheme,
        host => Domain,
        port => Port,
        path => "/api/" ++ Method
    }.

json_decode(JSON) ->
    jsx:decode(JSON, [{return_maps, true},{labels, binary}]).

headers(Instance) ->
    {ok, Token} = application:get_env(braid, braidnet_access_token),
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
