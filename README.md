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
    ./_build/default/bin/braid list ...
    ./_build/default/bin/braid logs ...
    ./_build/default/bin/braid rpc ...
    ...


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
