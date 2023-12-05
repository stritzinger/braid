% This is an utility module for easy generation of example configurations taylored on fly.io
-module(braid_config).

-export([gen/3]).

gen(mesh, DockerImage, Size) -> fully_connected_mesh(DockerImage, Size);
gen(ring, DockerImage, Size) -> ring(DockerImage, Size);
gen(hypercube, DockerImage, N) -> hypercube(DockerImage, N).

fully_connected_mesh(DockerImage, Size) ->
    Machines = fly_machines(),
    Sizes = divide_sizes_with_reminder(Size, length(Machines)),
    ParamsList = [{DockerImage, S, M} || {S,M} <- lists:zip(Sizes, Machines)],
    Configs = gen_containers_configs(ParamsList),
    Configs2 = interconnect_all_containers(Configs),
    CfgMap = maps:from_list([{M, maps:from_list(Cts)} || {M, Cts} <- Configs2]),
    ok = file:write_file("mesh.config", io_lib:format("~p.~n", [CfgMap])).

ring(DockerImage, Size) ->
    Machines = fly_machines(),
    Sizes = divide_sizes_with_reminder(Size, length(Machines)),
    ParamsList = [{DockerImage, S, M} || {S,M} <- lists:zip(Sizes, Machines)],
    Configs = gen_containers_configs(ParamsList),
    Configs2 = chain_all_containers(Configs),
    CfgMap = maps:from_list([{M, maps:from_list(Cts)} || {M, Cts} <- Configs2]),
    ok = file:write_file("ring.config", io_lib:format("~p.~n", [CfgMap])).

hypercube(DockerImage, N) ->
    Machines = fly_machines(),
    Vertices = gen_hypercube_vertices(N),
    Hypercube = connect_hypercube_vertices(N, Vertices),
    FormattedHypercube = hypercube_to_string(Hypercube),
    CfgMap = gen_hypercube_containers(DockerImage, Machines, FormattedHypercube),
    ok = file:write_file("hypercube.config", io_lib:format("~p.~n", [CfgMap])).

fly_machines() ->
    {200, Machines} = braid_rest:instances(),
    Machines.

divide_sizes_with_reminder(Total, BucketCount) ->
    Buckets = lists:seq(1, BucketCount, 1),
    [First|TL] = [Total div BucketCount || _ <- Buckets],
    [First + Total rem BucketCount | TL].

gen_containers_configs(ParamsList) ->
    do_gen_containers_configs(ParamsList, []).

do_gen_containers_configs([], Configs) -> Configs;
do_gen_containers_configs([{Img,Count,M} | Rest], Configs) ->
    do_gen_containers_configs(Rest, [{M, containers_configs(Img,Count)} | Configs]).

containers_configs(Image, Count) ->
    L = lists:seq(1, Count),
    [container(Image) || _ <- L].

container(Image) ->
    Name = list_to_binary(get_random_string(6,"abcdefghilmnopqrstuvzkxwy")),
    {Name,
        #{
            image => Image,
            connections => []
        }
    }.

get_random_string(Length, AllowedChars) ->
    lists:foldl(fun(_, Acc) ->
                        [lists:nth(rand:uniform(length(AllowedChars)),
                                   AllowedChars)]
                            ++ Acc
                end, [], lists:seq(1, Length)).

assemble_all_node_names(Configs) ->
    lists:append([assemble_node_names(M, Containers)
                   || {M, Containers} <- Configs]).

chain_all_containers(Configs) ->
    ContainersList = assemble_all_node_names(Configs),
    Neighbours = gen_ring_neighbours(ContainersList),
    {[], NewConfigs} = lists:foldl(
        fun({M, Containers}, {NList, Result}) ->
            {GroupN, OtherN} = lists:split(length(Containers), NList),
            NewContainers = [{Name, Opts#{connections => LocalNs}}
                || {{Name,Opts}, LocalNs} <- lists:zip(Containers, GroupN)],
            {OtherN, [{M, NewContainers} | Result]}
        end, {Neighbours, []}, Configs),
    NewConfigs.

interconnect_all_containers(Configs) ->
    ContainersList = assemble_all_node_names(Configs),
    [{M, store_connections(ContainersList, Containers)} || {M, Containers} <- Configs].

assemble_node_names(M, Containers) ->
    [iolist_to_binary([Name, "@", M])|| {Name, _} <- Containers].

store_connections(ContainersList, ContainersConfigs) ->
    % Maybe remove
    [{Name, Opts#{connections => ContainersList}}
        || {Name, Opts} <- ContainersConfigs].

gen_ring_neighbours([First, Second | _] = L) ->
    [Last, SecondLast | _] = lists:reverse(L),
    HeadAndTail = [[Last, Second], [SecondLast, First]],
    gen_ring_neighbours(HeadAndTail, L).

gen_ring_neighbours(Ring, [_SecondLast, _Last]) -> Ring;
gen_ring_neighbours(Ring, [Previous, Current, Next | TL]) ->
    gen_ring_neighbours([[Previous, Next]|Ring], [Current, Next |TL]).

% HYPERCUBE functions

gen_hypercube_vertices(N) ->
    Vertices = round(math:pow(2, N)),
    [V || V <- lists:seq(0, Vertices - 1)].

connect_hypercube_vertices(N, Vertices) ->
    Masks = [round(math:pow(2, E)) || E <- lists:seq(0, N - 1)],
    [{V, find_neighbours(V, Masks)} || V <- Vertices].

find_neighbours(Vertex, Masks) ->
    [Vertex bxor M || M <- Masks].

hypercube_to_string(Hypercube) ->
    [vertex_to_string(VE) ||VE <- Hypercube].

vertex_to_string({V, Neighbours}) ->
    {index_to_string(V), [index_to_string(E) || E <- Neighbours]}.

index_to_string(Integer) ->
    io_lib:format("~.2B", [Integer]).

gen_hypercube_containers(Image, Machines, Hypercube) ->
    Sizes = divide_sizes_with_reminder(length(Hypercube), length(Machines)),
    Occupancy = lists:zip(Sizes, Machines),
    rec_hypercube_containers(Image, Occupancy, Hypercube, #{}, []).

rec_hypercube_containers(Image, [], [], N2M, MachineConfigs) -> 
    maps:from_list([
        {M,
         maps:from_list([
            {Name, 
                #{
                    image => Image,
                    connections => [iolist_to_binary([N, "@", maps:get(N, N2M)]) 
                                        || N <- Conns]
                }
            } 
                || {Name, Conns} <- Containers])
        }
        || {M, Containers} <- MachineConfigs]);
rec_hypercube_containers(Image, [{Quantity, Machine} | Others], 
                         Hypercube, N2M, Cfg) ->
    {ToMachine, Rest} = lists:split(Quantity, Hypercube),
    NewN2M = maps:from_list([{N, Machine} || {N,_} <- ToMachine]),
    Containers = [{list_to_binary(Name), Connections}
                    || {Name, Connections} <- ToMachine],
    NewCfg = [{Machine, Containers} | Cfg],
    rec_hypercube_containers(Image, Others, Rest, maps:merge(NewN2M, N2M), NewCfg).
