% This is an utility module for easy generation of example configurations taylored on fly.io
-module(braid_config).

-export([fly_machines/0]).
-export([fully_connected_mesh/2]).
-export([fully_connected_mesh/3]).
-export([ring/2]).

-include_lib("stdlib/include/assert.hrl").


% Where containers start taking port numbers
-define(BASE_PORT_N, 40_000).


fully_connected_mesh(DockerImage, Size) ->
    fully_connected_mesh(DockerImage, Size, ?BASE_PORT_N).

fully_connected_mesh(DockerImage, Size, BasePort) ->
    Machines = fly_machines(),
    Sizes = divide_sizes_with_reminder(Size, length(Machines)),
    ParamsList = [{DockerImage, S, M, BasePort} || {S,M} <- lists:zip(Sizes, Machines)],
    Configs = gen_containers_configs(ParamsList),
    Configs2 = interconnect_all_containers(Configs),
    CfgMap = maps:from_list([{M, maps:from_list(Cts)} || {M, Cts} <- Configs2]),
    ok = file:write_file("examples/mesh.config", io_lib:format("~p.~n", [CfgMap])).


ring(DockerImage, Size) ->
    Machines = fly_machines(),
    Sizes = divide_sizes_with_reminder(Size, length(Machines)),
    ParamsList = [{DockerImage, S, M, ?BASE_PORT_N} || {S,M} <- lists:zip(Sizes, Machines)],
    Configs = gen_containers_configs(ParamsList),
    Configs2 = chain_all_containers(Configs),
    CfgMap = maps:from_list([{M, maps:from_list(Cts)} || {M, Cts} <- Configs2]),
    ok = file:write_file("examples/mesh.config", io_lib:format("~p.~n", [CfgMap])).

fly_machines() ->
    {ok, Domain} = application:get_env(braid, braidnet_domain),
    [AppName | _] = string:split(Domain, ".", all),
    Ret = os:cmd("fly machine list -a " ++  AppName ++ " -j"),
    Machines = json_decode(list_to_binary(Ret)),
    [ID || #{<<"id">> := ID} <- Machines].

json_decode(JSON) ->
    jsx:decode(JSON, [{return_maps, true},{labels, binary}]).

divide_sizes_with_reminder(Total, BucketCount) ->
    Buckets = lists:seq(1, BucketCount, 1),
    [First|TL] = [Total div BucketCount || _ <- Buckets],
    [First + Total rem BucketCount | TL].

gen_containers_configs(ParamsList) ->
    do_gen_containers_configs(ParamsList, []).

do_gen_containers_configs([], Configs) -> Configs;
do_gen_containers_configs([{Img,Count,M,P} | Rest], Configs) ->
    do_gen_containers_configs(Rest, [{M, containers_configs(Img,Count,P)} | Configs]).

containers_configs(Image, Count, BasePortN) ->
    L = lists:seq(BasePortN + 1, BasePortN + Count),
    [container(Image, PortN) || PortN <- L].

container(Image, PortN) ->
    Name = list_to_binary(get_random_string(6,"abcdefghilmnopqrstuvzkxwy")),
    {Name,
        #{
            image => Image,
            epmd_port => integer_to_binary(PortN),
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
