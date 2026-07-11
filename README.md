# hive-addon

Standalone `IAddon` contract — the addon/plugin abstraction as a leaf library.

A host loads addons that implement `IAddon` to gain capabilities (Dependency
Inversion): the host depends on this abstraction, never on the concrete addons,
and no addon compile-depends on the host. Any project — an MCP server or
something entirely unrelated — can host addons by consuming this lib.

## Coordinates

```clojure
io.github.hive-agi/hive-addon {:git/tag "v0.1.0" :git/sha "..."}
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

## Releasing

Bump `VERSION`, merge to `main` — CI tags `v<VERSION>` and cuts a GitHub release.

## License

MIT. Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW).
