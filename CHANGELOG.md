# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [1.0.0] - 2025-09-09

### Added
* Hypercube topology generator.
* `certifi` dependency to improve TLS CA handling for HTTPS.

### Changed
* README updates to reflect current CLI/API usage.

### Fixed
* Broken README link to the Braidnet project.

## [0.1.0] - 2023-07-01

### Added
* Allow arbitrary string flags when starting Erlang nodes.
* Support for interfacing with Braidnet.

## [0.0.1] - 2019-12-16

### Added
* `info/2` function that returns node metadata.
* `start/2` function which starts a node if stopped.
* `connect/2` function that reconnects a node in a cluster.
* `call/5` function which makes an RPC call to one node.
* `netload3` function that can load modules on only one node.

[Unreleased]: https://github.com/stritzinger/braid/compare/v1.0.0...master
[1.0.0]: https://github.com/stritzinger/braid/compare/v0.1.0...v1.0.0
[0.1.0]: https://github.com/stritzinger/braid/compare/v0.0.1...v0.1.0
[0.0.1]: https://github.com/stritzinger/braid/releases/tag/v0.0.1
