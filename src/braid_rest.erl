-module(braid_rest).

-export([instances/0]).
-export([launch/1]).
-export([list/1]).
-export([logs/2]).
-export([rpc/5]).
-export([destroy/1]).

% API --------------------------------------------------------------------------
%
instances() ->
    send_to_instance(get, undefined, "instances", []).

launch(ConfigOrPath) ->
    Config = parse_config(ConfigOrPath),
    Orchestrators = parse_instances(Config),
    [{Orch, send_to_instance(post, Orch, "launch", Config)} ||
        Orch <- Orchestrators].

list(ConfigOrPath) ->
    Config = parse_config(ConfigOrPath),
    Orchestrators = parse_instances(Config),
    [{Orch, send_to_instance(get, Orch, "list", [])} ||
        Orch <- Orchestrators].

logs(Instance, CID) ->
    send_to_instance(get, Instance, "logs", [{"cid", CID}]).

rpc(Instance, CID, M, F, A) ->
    BinM = base64:encode(term_to_binary(list_to_atom(M))),
    BinF = base64:encode(term_to_binary(list_to_atom(F))),
    {ok, Tokens, _} = erl_scan:string(A ++ "."),
    {ok, Term} = erl_parse:parse_term(Tokens),
    BinArgs = base64:encode(term_to_binary(Term)),
    QS = [{"cid", CID}, {"m", BinM}, {"f", BinF}, {"args", BinArgs}],
    {Code, Result} = send_to_instance(get, Instance, "rpc", QS),
    Msg = case Code of
        200 ->
            case is_binary(Result) of
                true -> base64:decode(Result);
                false -> Result
            end;
        _ -> Result
    end,
    {Code, Msg}.

destroy(ConfigOrPath) ->
    Config = parse_config(ConfigOrPath),
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
    {ok, Result} = httpc:request(Method, Request,
                                 http_opts(),
                                 [{body_format, binary}]),
    {{_HTTP, Code, Status}, _Headers, Reply} = Result,
    case Reply of
        <<>> -> {Code, Status};
        _ -> {Code, json_decode(Reply)}
    end.

parse_config(Config) when is_map(Config) ->
    Config;
parse_config(Path) ->
    {ok, [Config]} = file:consult(Path),
    Config.

parse_instances(Config) ->
    maps:keys(Config).

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

headers(undefined) ->
    {ok, Token} = application:get_env(braid, braidnet_access_token),
    [
        {"Authorization", "Bearer " ++ Token}
    ];
headers(Instance) when is_atom(Instance) ->
    headers(atom_to_list(Instance));
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
                {timeout, 10_000},
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
