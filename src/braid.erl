-module(braid).

-behavior(gen_server).

% API
-export([create/1]).
-export([multicall/4]).
-export([netload/2]).
-export([stop/1]).
-export([start_manager/2]).

% Callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

%--- API -----------------------------------------------------------------------

create(Nodes) ->
    Proxy = proxy_start(Nodes),
    for_each_node(fun(Node, #{args := Args}) ->
        start_link(Proxy, Node, Args)
    end, Nodes),
    for_each_node(fun(Node, Attrs) ->
        proxy_call(Proxy, {connect, Node, maps:get(connections, Attrs)})
    end, Nodes),
    Proxy.

multicall(Proxy, M, F, A) ->
    Nodes = proxy_call(Proxy, get_nodes),
    maps:fold(fun(Name, Attrs, Acc) ->
        Result = proxy_rpc(Proxy, mget(Attrs, [node]), M, F, A),
        maps:put(Name, Result, Acc)
    end, #{}, Nodes).

netload(Proxy, Module) ->
    ObjectCode = code:get_object_code(Module),
    proxy_call(Proxy, {netload, ObjectCode}).

stop(Proxy) ->
    proxy_call(Proxy, stop).

start_manager(Nodes, Module) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {Nodes, Module}, []).

%--- Callbacks -----------------------------------------------------------------

init({Nodes, Module}) -> {ok, #{nodes => Nodes, module => Module}}.

handle_call({start_link, Name, Args}, _From, State) ->
    {Node, _Port} = start_node(Name, Args),
    timer:sleep(1000),
    send_module(Node, mget(State, [module])),
    kill_switch(Node, self()),
    {reply, Node, mput(State, [nodes, Name, node], Node)};
handle_call({connect, Source, Targets}, _From, State) ->
    SourceNode = mget(State, [nodes, Source, node]),
    [connect(SourceNode, mget(State, [nodes, T, node])) || T <- Targets],
    {reply, ok, State};
handle_call(get_nodes, _From, State) ->
    {reply, mget(State, [nodes]), State};
handle_call({netload, ObjectCode}, _From, State) ->
    for_each_node(fun(_Node, Attrs) -> 
                          send_module(mget(Attrs, [node]), ObjectCode) 
                  end, mget(State, [nodes])),
    {reply, ok, State};
handle_call(stop, _From, State) ->
    init:stop(),
    {reply, ok, State};
handle_call(Request, From, _State) ->
    error({unknown_request, Request, From}).

handle_cast(Request, _State) -> error({unknown_cast, Request}).

handle_info(Info, _State) -> error({unknown_info, Info}).

%--- Internal ------------------------------------------------------------------

start_link(Proxy, Name, Args) ->
    proxy_call(Proxy, {start_link, Name, Args}).

connect(From, To) ->
    true = rpc(From, net_kernel, connect_node, [To]).

for_each_node(Fun, Nodes) ->
    maps:fold(fun(Node, Attrs, ok) ->
        Fun(Node, Attrs),
        ok
    end, ok, Nodes).

proxy_start(Nodes) ->
    {Node, _Port} = start_node(braid, [
        {noinput, true},
        {hidden, true}
    ]),
    timer:sleep(1000),
    Module = code:get_object_code(?MODULE),
    send_module(Node, Module),
    kill_switch(Node, self()),
    {ok, Pid} = rpc(Node, ?MODULE, start_manager, [Nodes, Module]),
    {Node, Pid}.

proxy_call({Node, Pid}, Request) ->
    rpc(Node, gen_server, call, [Pid, Request]).

proxy_rpc({Proxy, _Pid}, Node, M, F, A) ->
    rpc(Proxy, rpc, call, [Node, M, F, A]).

mget(Value, [])     -> Value;
mget(Map, [K|Keys]) -> mget(maps:get(K, Map), Keys).


mput(Map, [K], Value) ->
    maps:put(K, Value, Map);
mput(Map, [K|Keys], Value)  ->
    maps:put(K, mput(maps:get(K, Map), Keys, Value), Map).

start_node(Name, Attrs) ->
    N = atom_to_list(Name),
    Command = ["erl ", erl_flags([{sname, N}, {noinput, true}|Attrs])],
    Port = open_port({spawn, Command}, []),
    {ok, Host} = inet:gethostname(),
    Node = iolist_to_atom([N, "@", Host]),
    {Node, Port}.

erl_flags(Attrs) ->
    string:join([erl_flag(A) || A <- Attrs], " ").

erl_flag({noinput, true})      -> "-noinput";
erl_flag({sname, Name})        -> ["-sname ", Name];
erl_flag({hidden, true})       -> "-hidden";
erl_flag({connect_all, false}) -> "-connect_all false";
erl_flag({eval, Expr})         -> "-eval '" ++ Expr ++ "'";
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
