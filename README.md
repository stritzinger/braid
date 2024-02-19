# braid

### braid_cli.erl
Braid is a cli application as well as a client library to interface with running [braidnet](https://github.com/stritzinger/braidnet) instances in the cloud.

Currently we only support braidnet on fly.io, but in future, different cloud providers might be added.

### braid.erl

This project contains a single module library that acts locally using the OPT slave application.

## Use it as a CLI for your braidnet cloud

The CLI is the fastest way to use this client, generate the escript and use the setup command.

- braidnet domain (e.g. `braidnet.fly.dev`)
- the authenticaiton token used in REST requests

```
    rebar3 escriptize
    ./_build/default/bin/braid setup
````

Once your braidnet clould is online on Fly.io, you can generate a braidnet config.
We currently only have a `mesh` and `ring` setups with arbitrary scale.

    rebar3 escriptize
    ./_build/default/bin/braid config ring 4 my-hub/my-image:tag
    ./_build/default/bin/braid launch ring.config
    ./_build/default/bin/braid list ring.config
    ./_build/default/bin/braid logs <machine-id> <contianer-id>
    ./_build/default/bin/braid rpc <machine-id> <contianer-id> module function <args>
    ...

### CLI API commands

### config
    braid config ring 4 my-hub/my-image:tag

Generates a braidnet configuration for quick testing load balancing across the available braidnet machines. Connections between the instances follow a predefined topology between the ones available:

- `mesh`: fully connected mesh
- `ring`: a ring
- `hyperbube`: https://en.wikipedia.org/wiki/Hypercube

You can set the size and the docker image you want to use. The braidnet machine ids a queried just in time.

#### ring.config

```
#{<<"148e451b536dd8">> =>
      #{<<"zrnhok">> =>
            #{image => <<"my-repo/my-image:tag">>,
              connections =>
                  [<<"smumfu@2866e31ce15318">>,<<"xhlanz@e784e666f0d778">>]}},
  <<"2866e31ce15318">> =>
      #{<<"smumfu">> =>
            #{image => <<"my-repo/my-image:tag">>,
              connections =>
                  [<<"zrnhok@148e451b536dd8">>,<<"ysiped@91857556f71778">>]}},
  <<"91857556f71778">> =>
      #{<<"ysiped">> =>
            #{image => <<"my-repo/my-image:tag">>,
              connections =>
                  [<<"xhlanz@e784e666f0d778">>,<<"smumfu@2866e31ce15318">>]}},
  <<"e784e666f0d778">> =>
      #{<<"xhlanz">> =>
            #{image => <<"my-repo/my-image:tag">>,
              connections =>
                  [<<"ysiped@91857556f71778">>,<<"zrnhok@148e451b536dd8">>]}}}.
```

### launch
    braid launch mesh.config

Launches the configuration spawning the nodes on remote containers on the machines present in the configuration.

### destroy
    braid destroy mesh.config

Deletes the remote containers listed in the configuration on all machines.

### list
    braid list mesh.config

Lists the remote containers described in the configuration on all machines.
The container `id` and `status` is returned.
It can be either: `starting`, `running` or `lost`
```
{<<"148e451b536dd8">>,
 {200,
  [#{<<"id">> => <<"468dfc93-eb53-4c0f-8b69-958e62549459">>,
     <<"image">> => <<"ziopio/braidsquid:0.1.0">>,<<"name">> => <<"bob">>,
     <<"status">> => <<"running">>},
     ...
```
### logs
    braid logs <machine-id> <contianer-id>

After you runned the list command, you can user the `"id"` of any container and ask the remote machine for its logs. Braidnet observes its containers and caches their logs. This returns you the latest logs.

### rpc
    braid rpc <machine-id> <contianer-id> <module> <function> <args>

RPC to a selected container, this comes handy to test the topology by triggering an event in a particular instance inside the network.

`<args>` is a list of erlang terms, remember to use an escaped string:

    "[1, 3, atom, fun erlang:node/0, {my_tuple, \"lol\"}]"

## braid app as a library

You can use braid as library to integrate its functionalities in you own client.
Just take a look at the configuration file. You need to provide the correct entries as app env.

* set your braidnet domain env `{braidnet_domain, "***"}`
* set a proper authentication token env ` {braidnet_access_token, <<"***">>}`

You can also test this on the shell:

```
    rebar3 shell
    braid_config:gen(mesh, "my-hub/my-image:tag", 4).
    braid_rest:launch("mesh.config").
    braid_rest:list("mesh.config").
    ...
```


# braid module

`braid.erl` is an Erlang library to create and connect an arbitrary cluster of nodes.
The library is intended to be used for testing. It works in a similar mode to
`slave`, where nodes set up through the library are linked to the process
creating the cluster. Once that process terminates, all the nodes are cleaned
up.

## Model

Braid starts a manager process on the current node, which in turn is responsible
for setting up the cluster. The nodes started in the cluster connects back to
the current node via [hidden node connections][1]. If you want to connect
additional managing nodes to the cluster, it might be a good idea to make those
nodes hidden as well to prevent the test cluster from seeing unexpected new
normal nodes. This all depends on what you are testing of course.

## Example

Given the following configuriation:

```erlang
Nodes = #{
    n1 => #{
        args => [{connect_all, false}],
        connections => [n4, n2]
    },
    n2 => #{
        args => [{connect_all, false}],
        connections => [n1, n3]
    },
    n3 => #{
        args => [{connect_all, false}],
        connections => [n2, n4]
    },
    n4 => #{
        args => [{connect_all, false}],
        connections => [n3, n1]
    }
}.
```

You can create a cluster with:

```erlang
(manager@host)1> Cluster = braid:create(Nodes).
<0.87.0>
```

To check the cluster connections:

```
(manager@host)2> braid:multicall(Cluster, erlang, nodes, []).
#{n1 => ['n4@host','n2@host'],
  n2 => ['n1@host','n3@host'],
  n3 => ['n2@host','n4@host'],
  n4 => ['n1@host','n3@host']}
(manager@host)3> braid:multicall(Cluster, erlang, nodes, [hidden]).
#{n1 => ['manager@host'],
  n2 => ['manager@host'],
  n3 => ['manager@host'],
  n4 => ['manager@host']}
```

# Roadmap

- [ ] Support starting nodes on remote hosts
- [ ] Allow custom `erl` command line flags



[1]: http://erlang.org/doc/reference_manual/distributed.html#hidden-nodes
