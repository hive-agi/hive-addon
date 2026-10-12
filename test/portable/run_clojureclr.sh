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
dsl="${HIVE_DSL_SRC:-/home/klein/PP/hive/hive-dsl-wt/staging-ro/src}"
export CLOJURE_LOAD_PATH="$root/src:$root/test:$dsl"
for leg in oracle oracle_driver; do
  clojure -J-Xmx2g -Sdeps "{:paths [\"$root/src\" \"$root/test\" \"$dsl\"]}" -M -e "(require 'portable.${leg//_/-})" > "$out/$leg.jvm" 2>&1 || status=1
  "$clr" -e "(load-file \"$root/test/portable/$leg.cljc\")" > "$out/$leg.clr" 2>&1 || status=1
  grep -E ' \| |^ORACLE' "$out/$leg.jvm" > "$out/$leg.jvm.lines"
  grep -E ' \| |^ORACLE' "$out/$leg.clr" > "$out/$leg.clr.lines"
  if diff -u "$out/$leg.jvm.lines" "$out/$leg.clr.lines" > "$out/$leg.diff"; then
    echo "OK jvm == clojureclr $leg ($(wc -l < "$out/$leg.jvm.lines") lines)"
  else
    echo "FAIL jvm != clojureclr $leg"; cat "$out/$leg.diff"; status=1
  fi
done
exit "$status"
