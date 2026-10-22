#!/usr/bin/env bash
# Regenerates test/golden/*.txt by running the reference COBOL under GnuCOBOL 3.x.
# -fsign=EBCDIC makes DISPLAY signs use the {/A-I, }/J-R overpunch found in app/data.
set -euo pipefail
cd "$(dirname "$0")/../test/golden"
work=$(mktemp -d)
cobc -x -fsign=EBCDIC -o "$work/editgold" cobol/EDITGOLD.cbl
cobc -x -o "$work/fsgold" cobol/FSGOLD.cbl
"$work/editgold" > editgold.txt
(cd "$work" && ./fsgold) > fsgold.txt
rm -rf "$work"
