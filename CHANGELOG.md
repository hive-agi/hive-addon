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
