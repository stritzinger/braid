-module(braid).

-behavior(gen_server).

% API
-export([create/1]).
-export([info/2]).
-export([start/2]).
-export([connect/2]).
-export([multicall/4]).
-export([call/5]).
-export([netload/2]).
-export([netload/3]).
-export([stop/1]).
-export([stop/2]).

% Callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

%--- API -----------------------------------------------------------------------

create(Nodes) ->
    {ok, Manager} = gen_server:start_link({local, ?MODULE}, ?MODULE, Nodes, []),
    for_each_node(fun(Name, _Attrs) ->
        gen_server:call(Manager, {start_link, Name})
    end, Nodes),
    for_each_node(fun(Node, Attrs) ->
        gen_server:call(Manager, {connect, Node, maps:get(connections, Attrs)})
    end, Nodes),
    Manager.

info(Manager, Name) ->
    gen_server:call(Manager, {info, Name}).

start(Manager, Name) ->
    gen_server:call(Manager, {start_link, Name}).

connect(Manager, Name) ->
    Attrs = gen_server:call(Manager, {info, Name}),
    gen_server:call(Manager, {connect, Name, maps:get(connections, Attrs)}).

multicall(Manager, M, F, A) ->
    Nodes = gen_server:call(Manager, get_nodes),
    maps:fold(fun(Name, Attrs, Acc) ->
        Result = rpc:call(maps:get(node, Attrs), M, F, A),
        maps:put(Name, Result, Acc)
    end, #{}, Nodes).

call(Manager, Name, M, F, A) ->
    Attrs = gen_server:call(Manager, {info, Name}),
    rpc:call(maps:get(node, Attrs), M, F, A).

netload(Manager, Module) ->
    ObjectCode = code:get_object_code(Module),
    Nodes = gen_server:call(Manager, get_nodes),
    for_each_node(fun(_Node, Attrs) ->
        send_module(maps:get(node, Attrs), ObjectCode)
    end, Nodes),
    ok.

netload(Manager, Name, Module) ->
    ObjectCode = code:get_object_code(Module),
    Attrs = gen_server:call(Manager, {info, Name}),
    send_module(maps:get(node, Attrs), ObjectCode).

stop(Manager) -> stop(Manager, 5000).

stop(Manager, Timeout) ->
    ok = gen_server:call(Manager, stop, Timeout),
    Ref = erlang:monitor(process, Manager),
    receive
        {'DOWN', Ref, process, Manager, _Reason} ->
            ok
    after
        5000 ->
            error({manager_not_stopped, Manager})
    end.

%--- Callbacks -----------------------------------------------------------------

init(Nodes) -> {ok, #{nodes => Nodes}}.

handle_call({start_link, Name}, _From, State) ->
    case mapz:deep_get([nodes, Name], State) of
        #{status := alive} ->
            {reply, {error, already_started}, State};
        #{} ->
            Args = mapz:deep_get([nodes, Name, args], State),
            {Node, Monitor, Port} = start_node(Name, Args),
            Attrs = mapz:deep_get([nodes, Name], State),
            NewAttrs = Attrs#{
                node => Node,
                args => Args,
                port => Port,
                monitor => Monitor,
                status => alive
            },
            {reply, ok, mapz:deep_put([nodes, Name], NewAttrs, State)}
    end;
handle_call({connect, Source, Targets}, _From, State) ->
    SourceNode = mapz:deep_get([nodes, Source, node], State),
    [connect_nodes(SourceNode, mapz:deep_get([nodes, T, node], State)) || T <- Targets],
    {reply, ok, State};
handle_call({info, Node}, _From, State) ->
    {reply, mapz:deep_get([nodes, Node], State), State};
handle_call(get_nodes, _From, State) ->
    {reply, mapz:deep_get([nodes], State), State};
handle_call(stop, _From, #{nodes := Nodes} = State) ->
    for_each_node(fun(_Name, #{node := Node, port := Port}) ->
        rpc:call(Node, init, stop, []),
        Ref = erlang:monitor(port, Port),
        receive {'DOWN', Ref, port, Port, _Reason} -> ok
        after 5000 -> error({node_not_stopped, Node})
        end
    end, Nodes),
    {stop, normal, ok, State};
handle_call(Request, From, _State) ->
    error({unknown_request, Request, From}).

handle_cast(Request, _State) -> error({unknown_cast, Request}).

handle_info({Port, {data, Data}}, State) ->
    [#{node := Node}] = maps:values(maps:filter(fun
        (_N, #{port := P}) when P =:= Port -> true;
        (_N, _Attr) -> false
    end,  maps:get(nodes, State))),
    Lines = binary:split(Data, <<"\n">>),
    Formatted = [[atom_to_list(Node), L, $\n] || L <- Lines],
    io:format(Formatted),
    {noreply, State};
handle_info({'DOWN', Monitor, port, Port, Reason}, #{nodes := Nodes} = State) ->
    {value, {Node, _Attrs}} = lists:search(fun
        ({_Name, #{monitor := M, port := P}}) when M =:= Monitor, P =:= Port -> true;
        (_Node)                                                              -> false
    end, maps:to_list(Nodes)),
    {noreply, mapz:deep_put([nodes, Node, status], {down, Reason}, State)};
handle_info(Info, _State) ->
    error({unknown_info, Info}).

%--- Internal ------------------------------------------------------------------

connect_nodes(From, To) ->
    true = rpc(From, net_kernel, connect_node, [To]).

for_each_node(Fun, Nodes) ->
    maps:fold(fun(Node, Attrs, ok) ->
        Fun(Node, Attrs),
        ok
    end, ok, Nodes).

start_node(Name, Attrs) ->
    N = atom_to_list(Name),
    Current = atom_to_list(node()),
    Command = ["erl ", erl_flags([
        {sname, N},
        {noinput, true},
        {eval, [
            "net_kernel:hidden_connect_node('", Current, "'),"
            "erlang:send({braid, '", Current, "'}, node_started)"
        ]}
    |Attrs])],
    Port = open_port({spawn, Command}, [binary]),
    Monitor = receive node_started -> erlang:monitor(port, Port) end,
    {ok, Host} = inet:gethostname(),
    Node = iolist_to_atom([N, "@", Host]),
    Module = code:get_object_code(?MODULE),
    send_module(Node, Module),
    kill_switch(Node, self()),
    {Node, Monitor, Port}.

erl_flags(Attrs) ->
    string:join([erl_flag(A) || A <- Attrs], " ").

erl_flag({noinput, true})             -> "-noinput";
erl_flag({sname, Name})               -> ["-sname ", Name];
erl_flag({hidden, true})              -> "-hidden";
erl_flag({connect_all, false})        -> "-connect_all false";
erl_flag({eval, Expr})                -> "-eval \"" ++ Expr ++ "\"";
erl_flag({pa, Path})                  -> ["-pa ", Path];
erl_flag(String) when is_list(String) -> String.

kill_switch(Node, MonitoredProcess) ->
    rpc(Node, erlang, spawn, [fun() ->
        Ref = monitor(process, MonitoredProcess),
        receive
            {'DOWN', Ref, process, MonitoredProcess, _Reason} ->
                init:stop()
        end
    end]).

rpc(Node, Module, Function, Args) ->
    case rpc:call(Node, Module, Function, Args) of
        {badrpc, Reason} -> error({badrpc, Reason});
        Result           -> Result
    end.

iolist_to_atom(IOList) -> binary_to_atom(iolist_to_binary(IOList), utf8).

send_module(Node, {Module, Bin, File}) ->
    {module, Module} = rpc(Node, code, load_binary, [Module, File, Bin]),
    ok.
