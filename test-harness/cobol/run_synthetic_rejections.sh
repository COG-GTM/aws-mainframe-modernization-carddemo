#!/usr/bin/env bash
# Supplementary CBTRN01C scenario: the shipped sample data never takes the
# "CARD NUMBER ... COULD NOT BE VERIFIED" or "ACCOUNT ... NOT FOUND"
# branches (all 300 cards resolve).  This builds a 3-record fixture from
# the sample data so both rejection paths are captured in a golden:
#   rec 1: dailytran record 1 unchanged                   -> VERIFIED
#   rec 2: record 2 with card 9999999999999999            -> CARD_NOT_FOUND
#   rec 3: record 3 with card 8888888888888888, which is  -> ACCOUNT_NOT_FOUND
#          added to the xref pointing at account 99999999999 (not in acctdata)
# Outputs: work/synthetic-rejections/{fixtures,ksds,CBTRN01C}
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO="$(cd "$HERE/../.." && pwd)"
WORK="${WORK:-$HERE/work}"
SCEN="$WORK/synthetic-rejections"
FIX="$SCEN/fixtures"
mkdir -p "$FIX"
python3 - "$REPO/app/data/ASCII" "$FIX" <<'PY'
import sys
data, fix = sys.argv[1:3]
tran = open(data + '/dailytran.txt').read().split('\n')[:3]
tran[1] = tran[1][:262] + '9999999999999999' + tran[1][278:]
tran[2] = tran[2][:262] + '8888888888888888' + tran[2][278:]
assert all(len(t) == 350 for t in tran)
open(fix + '/dailytran.txt', 'w').write('\n'.join(tran) + '\n')
xref = open(data + '/cardxref.txt').read()
xref += '8888888888888888' + '000000099' + '99999999999' + '\n'
open(fix + '/cardxref.txt', 'w').write(xref)
print('fixtures written to', fix)
PY
export WORK KSDS="$SCEN/ksds" OUT="$SCEN/CBTRN01C"
XREF_SRC="$FIX/cardxref.txt" "$HERE/load_ksds.sh"
DALYTRAN_SRC="$FIX/dailytran.txt" "$HERE/run_cbtrn01c.sh"
