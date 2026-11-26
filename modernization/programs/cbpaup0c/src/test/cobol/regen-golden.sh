#!/usr/bin/env bash
# Regenerates src/test/resources/golden/* by running the ORIGINAL CBPAUP0C under GnuCOBOL 3.x.
# EXEC DLI blocks are rewritten (xform.py) into CALL 'DLISTUB', an in-memory DL/I stub over the
# sample DBPAUTP0 unload (converted by conv.py). Usage: src/test/cobol/regen-golden.sh (needs cobc).
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
module=$(cd "$here/../../.." && pwd)
repo=$(cd "$module/../../.." && pwd)
app="$repo/app/app-authorization-ims-db2-mq"
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
cd "$work"
python3 "$here/xform.py" "$app/cbl/CBPAUP0C.cbl" CBPAUP0G.cbl
python3 "$here/conv.py" "$module/src/test/resources/fixtures/AWS.M2.CARDDEMO.IMSDATA.DBPAUTP0.dat" segs.bin
cobc -free -m "$here/DLISTUB.cbl" -o DLISTUB.so
cobc -x -I "$app/cpy" CBPAUP0G.cbl -o CBPAUP0G
run() { # name yyyy/mm/dd sysin
  rm -f state.txt
  printf '%s\n' "$3" | COB_CURRENT_DATE="$2 10:00:00" COB_LIBRARY_PATH=. ./CBPAUP0G > "$module/src/test/resources/golden/$1.out"
  cp state.txt "$module/src/test/resources/golden/$1.state"
}
run jcl        2026/11/26 '00,00001,00001,Y'
run exp15      2023/11/20 '15,00002,00003,N'
run defaults   2023/11/20 'XX,     ,     ,N'
run chkp       2023/11/20 '05,00000,00000,N'
run zeroparm   2023/11/20 '7A,0    ,0    ,N'
run emptysysin 2024/01/02 ''
