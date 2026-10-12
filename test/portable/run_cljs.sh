#!/usr/bin/env bash
# ClojureScript (Node) differential leg, separate from run.sh.
#
# Each portable leg is compiled with cljs.main for Node and run there; its
# `label | value` and ORACLE lines must be byte-identical to the JVM's, the
# same contract run.sh holds cljw and cljrs to. Kanban ADDON-ORACLE-CLJS.
#
#   REQUIRE_HOSTS=1 bash test/portable/run_cljs.sh
#
# Env: CLJS_VERSION (default 1.12.42), NODE, LEGS (space-separated leg files)
set -uo pipefail
root="$(cd "$(dirname "$0")/../.." && pwd)"
node="${NODE:-$(command -v node || true)}"
cljs_version="${CLJS_VERSION:-1.12.42}"
legs="${LEGS:-preflight oracle oracle_driver oracle_opaque oracle_schema}"

if [ -z "$node" ]; then
  if [ "${REQUIRE_HOSTS:-0}" = 1 ]; then echo "FAIL node binary missing"; exit 1; fi
  echo "SKIP cljs (node not found; set REQUIRE_HOSTS=1 to make this a failure)"; exit 0
fi
echo "cljs : ClojureScript $cljs_version on node $("$node" -v)  [$node]"

cd "$root" || exit 1
# RELATIVE to the repo root on purpose: cljs's Node bootstrap joins the -d
# output dir onto the working directory, so an absolute /tmp path resolves to
# <root>/tmp/... and every leg fails with "Cannot find module".
out="target/portable-cljs"
rm -rf "$out"; mkdir -p "$out"
status=0
sdeps="{:paths [\"src\" \"test\"] :deps {org.clojure/clojurescript {:mvn/version \"$cljs_version\"}}}"

for leg in $legs; do
  ns="portable.${leg//_/-}"
  clojure -J-Xmx2g -Sdeps "$sdeps" -M -e "(require '$ns)" > "$out/$leg.jvm" 2>&1 || status=1
  if ! clojure -J-Xmx2g -Sdeps "$sdeps" -M -m cljs.main -t node \
         -d "$out/$leg.build" -o "$out/$leg.js" -c "$ns" > "$out/$leg.compile" 2>&1; then
    echo "FAIL cljs compile $leg"; tail -20 "$out/$leg.compile"; status=1; continue
  fi
  "$node" "$out/$leg.js" > "$out/$leg.cljs" 2>&1 || status=1
  grep -E ' \| |^ORACLE' "$out/$leg.jvm"  > "$out/$leg.jvm.lines"
  grep -E ' \| |^ORACLE' "$out/$leg.cljs" > "$out/$leg.cljs.lines"
  if [ ! -s "$out/$leg.jvm.lines" ]; then
    echo "FAIL jvm $leg printed nothing"; tail -20 "$out/$leg.jvm"; status=1
  elif diff -u "$out/$leg.jvm.lines" "$out/$leg.cljs.lines" > "$out/$leg.diff"; then
    echo "OK   jvm == cljs $leg ($(wc -l < "$out/$leg.jvm.lines") lines)"
  else
    echo "FAIL jvm != cljs $leg"; cat "$out/$leg.diff"; tail -5 "$out/$leg.cljs"; status=1
  fi
done
exit "$status"
