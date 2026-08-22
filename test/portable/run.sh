#!/usr/bin/env bash
# Tri-runtime differential oracle for the hive-addon portable stratum.
#
# Runs test/portable/oracle.cljc on the JVM, ClojureWasm and clojurust and diffs
# the three outputs. They must be IDENTICAL: the stratum's admission rules in
# hive-addon.mount.portable-test exist to keep this diff empty.
#
# The driver leg (oracle_driver.cljc) runs on JVM + cljw only; cljrs cannot
# implement a protocol from another namespace yet. See that file.
#
# Every host is fed the same SOURCE directories rather than deps.edn: cljw
# resolves only :git/url and :local/root coordinates, so a Maven-only dep is
# simply absent there. Portability is a property of the whole require closure,
# not of a file's extension.
#
#   HIVE_DSL_SRC=../hive-dsl/src CLJRS=/path/to/cljrs ./test/portable/run.sh
set -uo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
root="$(cd "$here/../.." && pwd)"

SRC="$root/src"
TEST="$root/test"
DSL="${HIVE_DSL_SRC:-$root/../hive-dsl/src}"
CLJW="${CLJW:-$(command -v cljw || true)}"
CLJRS="${CLJRS:-$(command -v cljrs || true)}"

[ -d "$DSL" ] || { echo "hive-dsl source not found at $DSL; set HIVE_DSL_SRC" >&2; exit 2; }

out="$(mktemp -d)"; trap 'rm -rf "$out"' EXIT
CP="$SRC:$DSL:$TEST"
status=0

run_host () { # name command...
  local name="$1"; shift
  if [ -z "${1:-}" ]; then echo "SKIP $name (binary not found)"; return; fi
  "$@" >"$out/$name" 2>&1
  # deps.edn notes and :paths warnings are host chatter, not observations
  grep -v -e '^note:' -e '^WARNING' "$out/$name" > "$out/$name.clean"
  mv "$out/$name.clean" "$out/$name"
}

echo "== portable.oracle =="
run_host jvm   clojure -Sdeps "{:paths [\"$SRC\" \"$DSL\" \"$TEST\"]}" -M -e "(require 'portable.oracle)"
[ -n "$CLJW" ]  && run_host cljw  "$CLJW"  -cp "$CP" "$here/oracle.cljc"
[ -n "$CLJRS" ] && run_host cljrs "$CLJRS" run --src-path "$SRC" --src-path "$DSL" --src-path "$TEST" "$here/oracle.cljc"

grep -q ORACLE-END "$out/jvm" || { echo "FAIL jvm did not complete"; cat "$out/jvm"; exit 1; }

for host in cljw cljrs; do
  [ -f "$out/$host" ] || continue
  if diff -u "$out/jvm" "$out/$host" > "$out/$host.diff"; then
    echo "OK   jvm == $host  ($(grep -c '|' "$out/jvm") observations)"
  else
    echo "FAIL jvm != $host"; cat "$out/$host.diff"; status=1
  fi
done

echo
echo "== portable.oracle-driver (JVM + cljw; cljrs blocked on cross-ns protocols) =="
run_host jvm-drv  clojure -Sdeps "{:paths [\"$SRC\" \"$DSL\" \"$TEST\"]}" -M -e "(require 'portable.oracle-driver)"
[ -n "$CLJW" ] && run_host cljw-drv "$CLJW" -cp "$CP" "$here/oracle_driver.cljc"

if [ -f "$out/cljw-drv" ]; then
  # the shared oracle.cljc prelude is required by both; compare only the driver leg
  for f in jvm-drv cljw-drv; do sed -n '/^remount\//,$p' "$out/$f" > "$out/$f.leg"; done
  if diff -u "$out/jvm-drv.leg" "$out/cljw-drv.leg" > "$out/drv.diff"; then
    echo "OK   jvm == cljw (driver leg)"
  else
    echo "FAIL jvm != cljw (driver leg)"; cat "$out/drv.diff"; status=1
  fi
fi

exit $status
