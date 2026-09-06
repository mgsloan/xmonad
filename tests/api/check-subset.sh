#!/usr/bin/env bash
#
# Assert that this fork's API is the intended subset of upstream xmonad's.
#
# Usage: tests/api/check-subset.sh
#
# Reads two checked-in goldens; builds nothing.  They come from different
# places, and that asymmetry is the point:
#
#   tests/api/river/     this fork, dumped from its own build
#                        stack build && GHC="stack exec -- ghc" tests/api/dump-api.sh tests/api/river
#
#   tests/api/upstream/  upstream xmonad, recorded from a real checkout
#                        tests/api/dump-api.sh tests/api/upstream ../xmonad
#
# Upstream is a recording rather than a build because this package no longer
# carries upstream's sources -- there is one source tree and it is the river
# one.  The assertion those sources existed to support is what survives them:
# a config or an xmonad-contrib module written against upstream's API compiles
# here, because everything absent is absent on purpose and written down.
#
# Three assertions, each of which has caught something worth catching:
#
#   1. Every name upstream exports and this fork does not is justified in
#      unportable.txt -- and every name justified there is genuinely absent.
#      The second half matters as much as the first: a stale entry means the
#      file is describing a backend that no longer exists.
#
#   2. This fork's eight modules add *nothing* to xmonad's API.  Everything it
#      offers that upstream does not lives in XMonad.River, which this check
#      does not look at -- so a config importing XMonad sees exactly the names
#      upstream offers, minus the unportable ones, and never more.  A
#      river-specific name appearing in XMonad.Core is a failure whose fix is
#      to move it to XMonad.River.  The comparison is against the union of
#      upstream's own surface and its Graphics.X11 re-exports, so a name that
#      merely moves between those two files is not flagged.
#
#   3. This fork re-exports nothing from a Graphics.X11-shaped compat surface.
#      This is the positive form of the rule the whole backend is built on: it
#      does not imitate an X11 that is not there.

set -euo pipefail

# sort and comm have to agree on an order, and under a UTF-8 locale they do
# not: GNU sort collates by the locale's rules while comm checks the order it
# is given, so this script fails with "comm: file 1 is not in sorted order"
# and compares nothing.  C collation is byte order -- stable across machines,
# libc versions and whatever the caller's environment happens to be.
export LC_ALL=C

cd "$(dirname "$0")/../.."

api=tests/api
x11_api=$api/upstream/xmonad-api.golden
x11_reexports=$api/upstream/xmonad-reexports.golden
river_api=$api/river/xmonad-api.golden
river_reexports=$api/river/xmonad-reexports.golden

for f in "$x11_api" "$x11_reexports" "$river_api" "$river_reexports"; do
    [ -f "$f" ] || { echo "check-subset: missing $f" >&2; exit 1; }
done

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

# Column 2 of a golden line is the declared name.
names() { cut -d'|' -f2 "$@" | sed 's/^ *//; s/ *$//' | sort -u; }

# A justification file is `name  reason`, with # comments and continuations.
listed() { grep -vE '^\s*(#|$)' "$1" | awk '{print $1}' | sort -u; }

names "$x11_api"                       > "$tmp/x11-api"
names "$x11_api" "$x11_reexports"      > "$tmp/x11-all"
names "$river_api"                     > "$tmp/river-api"

comm -23 "$tmp/x11-api"  "$tmp/river-api" > "$tmp/missing"
comm -13 "$tmp/x11-all"  "$tmp/river-api" > "$tmp/added"

listed "$api/unportable.txt" > "$tmp/unportable"

status=0

report() {
    local label=$1 actual=$2 claimed=$3 file=$4 verb=$5
    local undocumented documented_but_absent
    undocumented=$(comm -23 "$actual" "$claimed")
    documented_but_absent=$(comm -13 "$actual" "$claimed")

    if [ -n "$undocumented" ]; then
        echo "check-subset: $label not justified in $file:" >&2
        printf '  %s\n' $undocumented >&2
        status=1
    fi
    if [ -n "$documented_but_absent" ]; then
        echo "check-subset: $file lists names that are $verb:" >&2
        printf '  %s\n' $documented_but_absent >&2
        status=1
    fi
    if [ -z "$undocumented$documented_but_absent" ]; then
        printf 'check-subset: %-34s OK (%d)\n' "$label" "$(wc -l < "$actual")"
    fi
}

report "names dropped by the fork" "$tmp/missing" "$tmp/unportable" \
       "$api/unportable.txt" "not actually dropped"

if [ -s "$tmp/added" ]; then
    echo "check-subset: these have no X11 counterpart and belong in XMonad.River:" >&2
    sed 's/^/  /' "$tmp/added" >&2
    status=1
else
    printf 'check-subset: %-34s OK (0)\n' "names added by the fork"
fi

if [ -s "$river_reexports" ]; then
    echo "check-subset: the fork re-exports an X11-shaped compat surface:" >&2
    head -20 "$river_reexports" >&2
    echo "  this fork must not imitate Graphics.X11; see tests/api/unportable.txt" >&2
    status=1
else
    printf 'check-subset: %-34s OK (0)\n' "X11 re-exports"
fi

exit "$status"
