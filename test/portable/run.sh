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
#
# Env: CLJW, CLJRS, HIVE_DSL_SRC, MALLI_SRC, DYNALOAD_SRC   — binary/source paths
#      CLJW_MIN, CLJ_MIN                                    — version floors
#      REQUIRE_HOSTS=1                                       — a missing host FAILS
set -uo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
root="$(cd "$here/../.." && pwd)"

SRC="$root/src"
TEST="$root/test"
DSL="${HIVE_DSL_SRC:-$root/../hive-dsl/src}"
# Prefer a repo-built cljw over whatever is on PATH. A STALE cljw is the worst
# outcome here: it silently answers for a version nobody is shipping, and every
# "identical" line it produces is evidence about the wrong binary. This bit us —
# a brew cljw 16 patch versions behind PATH-shadowed the current build and made
# a long-fixed macro-hygiene bug look like an open blocker.
CLJW="${CLJW:-}"
if [ -z "$CLJW" ]; then
  for cand in "$root/../../ClojureWasm/zig-out/bin/cljw" "$(command -v cljw || true)"; do
    [ -x "$cand" ] && { CLJW="$cand"; break; }
  done
fi
CLJRS="${CLJRS:-$(command -v cljrs || true)}"

# Version FLOORS, not just a printed header. Printing catches a stale binary
# only when a human reads the header; unattended, nobody does, and the run goes
# green against the wrong runtime. Raise a floor when a fix this oracle depends
# on lands upstream.
CLJW_MIN="${CLJW_MIN:-1.10.18}"
CLJ_MIN="${CLJ_MIN:-1.11.0}"
# A missing host SKIPs locally (not everyone has every runtime built) and FAILS
# under REQUIRE_HOSTS=1, which CI sets. "SKIP" inside an otherwise-green log is
# indistinguishable from a pass at a glance, so unattended runs must not have it.
REQUIRE_HOSTS="${REQUIRE_HOSTS:-0}"

[ -d "$DSL" ] || { echo "hive-dsl source not found at $DSL; set HIVE_DSL_SRC" >&2; exit 2; }

out="$(mktemp -d)"; trap 'rm -rf "$out"' EXIT
CP="$SRC:$DSL:$TEST"
status=0

fail () { echo "FAIL $*"; status=1; }

# true when $1 sorts strictly before $2 under version ordering
version_lt () {
  [ "$1" = "$2" ] && return 1
  [ "$(printf '%s\n%s\n' "$1" "$2" | sort -V | head -1)" = "$1" ]
}

assert_version () { # label actual floor
  if [ -z "$2" ]; then fail "$1 version unreadable — refusing to treat it as current"; return; fi
  if version_lt "$2" "$3"; then fail "$1 $2 is BELOW the pinned floor $3"; fi
}

run_host () { # name command...
  local name="$1"; shift
  if [ -z "${1:-}" ]; then return 1; fi
  "$@" >"$out/$name" 2>&1
  # deps.edn notes and :paths warnings are host chatter, not observations
  grep -v -e '^note:' -e '^WARNING' "$out/$name" > "$out/$name.clean"
  mv "$out/$name.clean" "$out/$name"
}

have_host () { # label binary
  if [ -n "$2" ]; then return 0; fi
  if [ "$REQUIRE_HOSTS" = "1" ]; then
    fail "$1 binary not found (REQUIRE_HOSTS=1)"
  else
    echo "SKIP $1 (binary not found; set REQUIRE_HOSTS=1 to make this a failure)"
  fi
  return 1
}

# =============================================================================
# Host admission — assert before measuring
# =============================================================================

CLJ_VER="$(clojure -M -e '(println (clojure-version))' 2>/dev/null | tr -d '[:space:]')"
echo "jvm  : clojure $CLJ_VER  (floor $CLJ_MIN)"
assert_version "jvm clojure" "$CLJ_VER" "$CLJ_MIN"

if have_host cljw "$CLJW"; then
  CLJW_RAW="$("$CLJW" --version 2>&1 | head -1)"
  CLJW_VER="$(printf '%s' "$CLJW_RAW" | sed -nE 's/.*[vV]([0-9]+\.[0-9]+\.[0-9]+).*/\1/p')"
  echo "cljw : $CLJW_RAW  (floor $CLJW_MIN)  [$CLJW]"
  assert_version "cljw" "$CLJW_VER" "$CLJW_MIN"
fi

if have_host cljrs "$CLJRS"; then
  echo "cljrs: $("$CLJRS" --version 2>&1 | head -1)  [$CLJRS]"
  # No floor: cljrs reports its Cargo version ("cljrs 0.1.0") unchanged across
  # every build, so the string carries no staleness signal at all. The preflight
  # leg below is its assertion — it pins the CONSTRUCTS this stratum depends on,
  # which is what a version floor is a proxy for anyway.
fi

[ $status -eq 0 ] || { echo; echo "host admission failed — not running the oracle against runtimes it does not trust"; exit 1; }

# =============================================================================
# Preflight — language capability, before any hive-addon code loads
# =============================================================================
# Requires nothing from hive-addon, so it still answers when the stratum will not
# load, and a failure names the CONSTRUCT rather than a downstream behavioural
# diff. This is the assertion standing in for cljrs's absent version.

echo
echo "== portable.preflight =="
run_host jvm-pre clojure -Sdeps "{:paths [\"$SRC\" \"$DSL\" \"$TEST\"]}" -M -e "(require 'portable.preflight)"
grep -q PREFLIGHT-END "$out/jvm-pre" || { echo "FAIL jvm preflight did not complete"; cat "$out/jvm-pre"; exit 1; }

[ -n "$CLJW" ]  && run_host cljw-pre  "$CLJW"  -cp "$CP" "$here/preflight.cljc"
[ -n "$CLJRS" ] && run_host cljrs-pre "$CLJRS" run --src-path "$SRC" --src-path "$DSL" --src-path "$TEST" "$here/preflight.cljc"

for host in cljw cljrs; do
  [ -f "$out/$host-pre" ] || continue
  if diff -u "$out/jvm-pre" "$out/$host-pre" > "$out/$host-pre.diff"; then
    echo "OK   jvm == $host  ($(grep -c '|' "$out/jvm-pre") constructs)"
  else
    fail "jvm != $host (preflight)"; cat "$out/$host-pre.diff"
  fi
done

echo
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
    fail "jvm != $host"; cat "$out/$host.diff"
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
    fail "jvm != cljw (driver leg)"; cat "$out/drv.diff"
  fi
fi

echo
echo "== portable.oracle-opaque (JVM + cljw; the tier a proprietary kernel is BUILT from) =="
# The two namespaces a `cljw build` artifact loads. cljw is not one host among
# three here, it is THE host: the kernel is a cljw binary, so a divergence in
# this diff means a customer gets different answers from the binary than from
# the source it was built from.
#
# It needs no malli and no hive-dsl — that is the tier's admission rule, and
# hive-addon.mount.portable-test asserts it off the require closure — so this
# leg runs whenever cljw does.
run_host jvm-opq clojure -Sdeps "{:paths [\"$SRC\" \"$DSL\" \"$TEST\"]}" -M -e "(require 'portable.oracle-opaque)"
grep -q ORACLE-OPAQUE-END "$out/jvm-opq" || { echo "FAIL jvm opaque leg did not complete"; cat "$out/jvm-opq"; exit 1; }
[ -n "$CLJW" ] && run_host cljw-opq "$CLJW" -cp "$CP" "$here/oracle_opaque.cljc"

if [ -f "$out/cljw-opq" ]; then
  grep -q ORACLE-OPAQUE-END "$out/cljw-opq" || fail "cljw opaque leg did not complete"
  if diff -u "$out/jvm-opq" "$out/cljw-opq" > "$out/opq.diff"; then
    echo "OK   jvm == cljw (opaque leg, $(grep -c '|' "$out/jvm-opq") observations)"
  else
    fail "jvm != cljw (opaque leg)"; cat "$out/opq.diff"
  fi
fi

echo
echo "== portable.oracle-schema (JVM + cljw; cljrs blocked on deftype) =="
# cljw resolves :git/url + :local/root only, so malli must be on the classpath as
# SOURCE. Discover the cljw gitlib checkout; skip the leg (loudly) if absent —
# silently skipping is how a stale/absent arm gets mistaken for a passing one.
MALLI_SRC="${MALLI_SRC:-$(ls -d "$HOME"/.cljw/gitlibs/malli/*/src 2>/dev/null | head -1)}"
DYNALOAD_SRC="${DYNALOAD_SRC:-$(ls -d "$HOME"/.cljw/gitlibs/dynaload/*/src 2>/dev/null | head -1)}"

run_host jvm-sch clojure -Sdeps "{:paths [\"$SRC\" \"$DSL\" \"$TEST\"] :deps {metosin/malli {:mvn/version \"0.20.1\"}}}" -M -e "(require 'portable.oracle-schema)"
if [ -n "$CLJW" ] && [ -d "${MALLI_SRC:-}" ] && [ -d "${DYNALOAD_SRC:-}" ]; then
  run_host cljw-sch "$CLJW" -cp "$CP:$MALLI_SRC:$DYNALOAD_SRC" "$here/oracle_schema.cljc"
  for f in jvm-sch cljw-sch; do sed -n '/^ms\//,$p' "$out/$f" > "$out/$f.leg"; done
  if diff -u "$out/jvm-sch.leg" "$out/cljw-sch.leg" > "$out/sch.diff"; then
    echo "OK   jvm == cljw (schema leg, $(grep -c '|' "$out/jvm-sch.leg") observations)"
  else
    fail "jvm != cljw (schema leg)"; cat "$out/sch.diff"
  fi
elif [ "$REQUIRE_HOSTS" = "1" ]; then
  fail "cljw schema leg cannot run (malli/dynaload source not found) and REQUIRE_HOSTS=1"
else
  echo "SKIP cljw schema leg (malli/dynaload source not found; set MALLI_SRC + DYNALOAD_SRC)"
fi

exit $status
