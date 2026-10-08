#!/usr/bin/env bash
# ClojureCLR differential leg, separate from run.sh.
# REQUIRE_HOSTS=1 bash test/portable/run_clojureclr.sh
set -uo pipefail
root="$(cd "$(dirname "$0")/../.." && pwd)"
clr="${CLOJURE_MAIN:-$HOME/.local/share/hive/dialects/clr/Clojure.Main}"
export DOTNET_ROOT="${DOTNET_ROOT:-$HOME/.local/share/hive/dialects/dotnet}"
export PATH="$(dirname "$clr"):$DOTNET_ROOT:$PATH"
if [ ! -x "$clr" ]; then
  if [ "${REQUIRE_HOSTS:-0}" = 1 ]; then echo "FAIL clojureclr binary missing: $clr"; exit 1; fi
  echo "SKIP clojureclr binary missing: $clr"; exit 0
fi
ver="$("$clr" -e '(println (clojure-version))' 2>&1 | tail -1)"
echo "clojureclr: Clojure $ver [$clr]"
if [ "$ver" != 1.12.2 ]; then echo "FAIL clojureclr version expected 1.12.2"; exit 1; fi
out="$(mktemp -d)"; trap 'rm -rf "$out"' EXIT
status=0
cd "$root" || exit 1
clojure -J-Xmx2g -Sdeps "{:paths [\"$root/test\"]}" -M -e "(require 'portable.preflight)" > "$out/jvm" 2>&1 || status=1
"$clr" "$root/test/portable/preflight.cljc" > "$out/clr" 2>&1 || status=1
grep -v '^Clojure core loaded' "$out/clr" > "$out/clr.clean"
if diff -u "$out/jvm" "$out/clr.clean" > "$out/preflight.diff"; then
  echo "OK jvm == clojureclr preflight ($(grep -c '|' "$out/jvm") constructs)"
else
  echo 'FAIL jvm != clojureclr preflight'; cat "$out/preflight.diff"; status=1
fi
echo 'SKIP portable.oracle: hive-dsl.result/rescue-fn catches :default as CLR class (ParseException line 352)'
echo 'SKIP oracle-driver: portable.oracle prelude cannot load'
if [ "${REQUIRE_HOSTS:-0}" = 1 ]; then status=1; fi
exit "$status"
