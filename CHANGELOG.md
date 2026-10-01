# Changelog

Notable changes to hive-addon. Format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/); versioning follows
[Semantic Versioning](https://semver.org/spec/v2.0.0.html).

This file starts at 1.0.0. Earlier history is in the git log and the release
tags (v0.1.0 through v0.3.12).

## What the version number promises

The public contract is `IAddon` and its companion protocols (`ITerminalAddon`,
`IVessel`), the mount and plug manifest schemas, and the registry APIs.

Removing a protocol or a manifest key, or adding a required method or key, is
major: every addon in the fleet must be edited to stay mountable. Adding an
optional manifest key, an optional companion protocol, or a registry function
is minor. A host adding capabilities, or an addon ignoring capabilities it does
not declare, is neither.

## [Unreleased]

### Added

- `hive-addon.tool-contract`: the contract a tool def must meet to be served
  at an MCP root — non-blank `:name` and `:description`, and an `:inputSchema`
  of type `"object"` with at least one property. A client with no source access
  can only operate a tool through its schema, so an empty schema is an error.
  A property without a `:description` is reported as a warning.
  - `assert-root-tool!` / `assert-root-tools!` for hosts at registration.
  - `def-root-tool` checks a literal tool map at macroexpansion, so a violation
    fails compilation, and checks the evaluated value when the def runs.
  - `hive-addon.tool-contract.test/deftest-root-tools` for an addon's suite.
  - `hive-addon.tool-contract.check` CLI and the reusable workflow
    `.github/workflows/root-tool-contract.yml` for an addon's CI.

- Every opaque-kernel request is bounded by a deadline. Two optional keys on
  the spec carried as `:addon/config` set them: `:opaque/init-timeout-ms` for
  the first request after each kernel start (default 60000, since a cold
  kernel pays its startup there) and `:opaque/request-timeout-ms` for every
  later one (default 30000). Both are optional, so an addon that declares
  neither keeps working unchanged.

  A kernel that misses a deadline is STOPPED rather than reused: a subprocess
  that has already failed to answer once has no readable position in the line
  protocol, and a later reply would be read as the answer to a different
  request. Calls against a stopped kernel fail with an error naming the
  deadline it missed, `health` reports `:down` with that reason, and the next
  `initialize!` starts a fresh kernel.

  The stop kills the whole process TREE, not just the direct child: a kernel
  launched through a wrapper leaves the real worker running otherwise, and it
  keeps holding the pipe.

## [1.0.0]

The contract stopped moving. Nothing in it changed for this release. 1.0.0 is
the promise that a mounted addon keeps mounting, made because hive-mcp cannot
promise stability over a contract that does not promise it.

The seam as it stands at 1.0.0:

- `hive-addon.protocol/IAddon`: `addon-id`, `addon-type`, `capabilities`,
  `initialize!`, `shutdown!`, `tools`, `schema-extensions`, `resources`,
  `hooks`, `excluded-tools`, `health`.
- `hive-addon.terminal/ITerminalAddon` and `hive-addon.vessel/IVessel`:
  companion protocols implemented on the same reify as `IAddon`, so a terminal
  backend or a headed environment ships as an addon without compile-depending
  on a host.
- `hive-addon.mount` / `hive-addon.plug`: manifest discovery, dependency
  solving, bounded init retry (`:addon/init-retry`), entitlement gating.
- `hive-addon.hot`: remount from a changed manifest, cascading to every addon
  handed the old instance, refusing a reload whose namespace pass provably did
  not happen (`:hot/stale-ctors`).
- `hive-addon.opaque`: mount a compiled, source-free addon through the generic
  proxy, as an ordinary `:external` manifest under the licence gate.

### Added

- `CHANGELOG.md` (this file), plus `## Companion contracts` and
  `## Versioning` sections in the README covering `ITerminalAddon`, `IVessel`,
  the capability manifest and the opaque seam, none of which the README
  mentioned.

### Fixed

- The README's coordinate example still said `0.3.2`, eleven releases behind.
