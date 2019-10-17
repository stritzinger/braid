# braid

Braid is an Erlang library to create and connect an arbitrary cluster of nodes.
The library is intended to be used for testing. It works in a similar mode to
`slave`, where nodes set up through the library are linked to the process
creating the cluster. Once that process terminates, all the nodes are cleaned
up.

## Model

Braid creates a hidden proxy node called `braid` that sets up all other nodes
and maintains connection to them. The reason is to avoid leaking node
connections which would enlarge the cluster by mistake, or force the user to use
the library from a hidden node.

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
1> Cluster = braid:create(Nodes).
{'braid@host',<16361.87.0>}
```

To check the cluster connections:

```
2> braid:multicall(Cluster, erlang, nodes, []).
#{n1 => ['n4@host','n2@host'],
  n2 => ['n1@host','n3@host'],
  n3 => ['n2@host','n4@host'],
  n4 => ['n1@host','n3@host']}
```

# Roadmap

- [ ] Support starting nodes on remote hosts
- [ ] Allow custom `erl` command line flags
