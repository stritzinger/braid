# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

* Allow arbitrary string flags when starting Erlang nodes.

## [0.0.1] - 2019-12-16

* Added `info/2` function that returns node metadata.
* Added `start/2` function which starts a node if stopped.
* Added `connect/2` function that reconnects a node in a cluster.
* Added `call/5` function which makes an RPC call to one node.
* Added `netload3` function that can load modules on only one node.

[Unreleased]: https://github.com/stritzinger/braid/compare/v0.0.1...master
[0.0.1]: https://github.com/stritzinger/braid/releases/tag/v0.0.1
