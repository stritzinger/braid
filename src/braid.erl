-module(braid).

-behavior(gen_server).

% API
-export([create/1]).
-export([multicall/4]).
-export([netload/2]).
-export([stop/1]).

% Callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

%--- API -----------------------------------------------------------------------

create(Nodes) ->
    {ok, Manager} = gen_server:start_link({local, ?MODULE}, ?MODULE, Nodes, []),
    for_each_node(fun(Name, #{args := Args}) ->
        gen_server:call(Manager, {start_link, Name, Args})
    end, Nodes),
    for_each_node(fun(Node, Attrs) ->
        gen_server:call(Manager, {connect, Node, maps:get(connections, Attrs)})
    end, Nodes),
    Manager.

multicall(Manager, M, F, A) ->
    Nodes = gen_server:call(Manager, get_nodes),
    maps:fold(fun(Name, Attrs, Acc) ->
        Result = rpc:call(mget(Attrs, [node]), M, F, A),
        maps:put(Name, Result, Acc)
    end, #{}, Nodes).

netload(Manager, Module) ->
    ObjectCode = code:get_object_code(Module),
    Nodes = gen_server:call(Manager, get_nodes),
    for_each_node(fun(_Node, Attrs) ->
        send_module(mget(Attrs, [node]), ObjectCode)
    end, Nodes),
    ok.

stop(Manager) ->
    ok = gen_server:call(Manager, stop, 60000),
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

handle_call({start_link, Name, Args}, _From, State) ->
    {Node, Port} = start_node(Name, Args),
    Module = code:get_object_code(?MODULE),
    send_module(Node, Module),
    kill_switch(Node, self()),
    Attrs = mget(State, [nodes, Name]),
    NewAttrs = Attrs#{node => Node, port => Port},
    {reply, Node, mput(State, [nodes, Name], NewAttrs)};
handle_call({connect, Source, Targets}, _From, State) ->
    SourceNode = mget(State, [nodes, Source, node]),
    [connect(SourceNode, mget(State, [nodes, T, node])) || T <- Targets],
    {reply, ok, State};
handle_call(get_nodes, _From, State) ->
    {reply, mget(State, [nodes]), State};
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

handle_info({_Port, {data, Data}}, State) -> 
    io:format(Data),
    {noreply, State};
handle_info(Info, _State) -> 
    error({unknown_info, Info}).

%--- Internal ------------------------------------------------------------------

connect(From, To) ->
    true = rpc(From, net_kernel, connect_node, [To]).

for_each_node(Fun, Nodes) ->
    maps:fold(fun(Node, Attrs, ok) ->
        Fun(Node, Attrs),
        ok
    end, ok, Nodes).

mget(Value, [])     -> Value;
mget(Map, [K|Keys]) -> mget(maps:get(K, Map), Keys).


mput(Map, [K], Value) ->
    maps:put(K, Value, Map);
mput(Map, [K|Keys], Value)  ->
    maps:put(K, mput(maps:get(K, Map), Keys, Value), Map).

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
    Port = open_port({spawn, Command}, []),
    receive
        node_started -> ok
    end,
    {ok, Host} = inet:gethostname(),
    Node = iolist_to_atom([N, "@", Host]),
    {Node, Port}.

erl_flags(Attrs) ->
    string:join([erl_flag(A) || A <- Attrs], " ").

erl_flag({noinput, true})      -> "-noinput";
erl_flag({sname, Name})        -> ["-sname ", Name];
erl_flag({hidden, true})       -> "-hidden";
erl_flag({connect_all, false}) -> "-connect_all false";
erl_flag({eval, Expr})         -> "-eval \"" ++ Expr ++ "\"";
erl_flag({pa, Path})           -> ["-pa ", Path].

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
