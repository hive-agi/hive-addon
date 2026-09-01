# hive-addon

<!-- hive-badges -->

[![Clojars Project](https://img.shields.io/clojars/v/io.github.hive-agi/hive-addon.svg)](https://clojars.org/io.github.hive-agi/hive-addon)
[![cljdoc](https://cljdoc.org/badge/io.github.hive-agi/hive-addon)](https://cljdoc.org/d/io.github.hive-agi/hive-addon/CURRENT)
[![release](https://github.com/hive-agi/hive-addon/actions/workflows/release.yml/badge.svg)](https://github.com/hive-agi/hive-addon/actions/workflows/release.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

<!-- /hive-badges -->

Standalone `IAddon` contract — the addon/plugin abstraction as a leaf library.

A host loads addons that implement `IAddon` to gain capabilities (Dependency
Inversion): the host depends on this abstraction, never on the concrete addons,
and no addon compile-depends on the host. Any project — an MCP server or
something entirely unrelated — can host addons by consuming this lib.

## Coordinates

```clojure
io.github.hive-agi/hive-addon {:mvn/version "0.3.2"}
```

Deps: Clojure + [hive-dsl](https://github.com/hive-agi/hive-dsl) + [malli](https://github.com/metosin/malli). `.cljc`.

## The contract

`hive-addon.protocol/IAddon`:

| Method | Returns |
|---|---|
| `addon-id` | stable string identifier (registry key) |
| `addon-type` | `:native` \| `:mcp-bridge` \| `:external` |
| `capabilities` | set of capability keywords |
| `initialize!` / `shutdown!` | idempotent lifecycle |
| `health` | `{:status :ok\|:degraded\|:down}` |
| `tools` / `schema-extensions` / `resources` / `hooks` | host contributions |
| `excluded-tools` | tool names this addon supersedes |

Plus predicates (`addon?`, `valid-addon-type?`, `healthy?`/`degraded?`/`down?`)
and constants (`valid-addon-types`, `standard-capabilities`, `health-statuses`).

```clojure
(require '[hive-addon.protocol :as addon])

(defrecord MyAddon []
  addon/IAddon
  (addon-id     [_] "my.addon")
  (addon-type   [_] :native)
  (capabilities [_] #{:tools})
  (initialize!  [_ _config] {:success? true})
  (shutdown!    [_] nil)
  (tools        [_] [...])
  (schema-extensions [_] [])
  (health       [_] {:status :ok})
  (excluded-tools [_] #{})
  (hooks        [_] {}))
```

## Reliable mounting

Mount manifests may request bounded initialization retries:

~~~clojure
{:addon/id "my.addon"
 :addon/type :native
 :addon/init-ns "my.addon"
 :addon/init-fn "addon-ctor"
 :addon/init-retry {:max-attempts 4
                    :initial-delay-ms 250
                    :max-delay-ms 2000
                    :backoff-factor 2}}
~~~

hive-addon.mount/mount-classpath! and
hive-addon.mount/compose-classpath! accept :on-event for structured lifecycle
events and :sleep-fn for deterministic tests. Mount results report
:init-attempts.

## Hot reload and injection

`hive-addon.hot` rebuilds a mounted addon from its manifest when its code
changes, and cascades to every addon that was handed its instance. hive-hot is
a SOFT dependency: with it on the classpath the namespace reload is delegated
to `hive-hot.core/reload-scoped!`, scoped to the seeds' own source roots: a
change another session left under some other watched root is declined and
reported under `:hot/ns-skipped`, never loaded on the caller's behalf.

~~~clojure
(require '[hive-addon.hot :as hot])

(hot/reload-addon! host specs "my.addon" {:mount-opts {:resolve-config my-resolver}})
;; => RemountReport: :hot/affected, :hot/torn-down, :mounted,
;;    :hot/ns-reloaded / :hot/ns-skipped / :hot/ns-dragged, :hot/stale-ctors ...
~~~

A reload whose namespace pass claims a load that did not happen (the
constructor var is provably the same object afterwards) is REFUSED
(`:hot/stale-ctors`) instead of remounting from old code and reporting success.

`hive-addon.hot.inject/inject!` mounts an addon that was not on the classpath
at boot: it puts the project's `deps.edn :paths` (or a source dir, or a jar) on
the live DynamicClassLoader, discovers the manifests under those paths only,
solves them against the mounted peers, remounts the mounted dependents that
now have a new sibling to receive, and registers the new addons with hive-hot.

~~~clojure
(require '[hive-addon.hot.inject :as inject])

(inject/inject! host mounted-specs "/path/to/addon-project"
                {:mount-opts {:resolve-config my-resolver}})
;; => InjectReport: :hot/injected, :hot/already-mounted, :hot/affected, :mounted ...
~~~

## Releasing

Bump `VERSION`, merge to `main` — CI tags `v<VERSION>` and cuts a GitHub release.

## License

MIT. Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW).
