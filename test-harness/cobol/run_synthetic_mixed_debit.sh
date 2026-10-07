#!/usr/bin/env bash
# Supplementary CBACT01C scenario: every shipped account has a zero
# ACCT-CURR-CYC-DEBIT, so the "input is not zero" branch of
#     IF ACCT-CURR-CYC-DEBIT EQUAL TO ZERO MOVE 2525.00 TO OUT-ACCT-CURR-CYC-DEBIT
# is never exercised.  This builds a 5-account fixture from the sample data:
#   acct 1: debit  10.00 (non-zero on the very first record)
#   acct 2: zero            -> 2525.00
#   acct 3: debit 120.50 (non-zero after a 2525.00 record)
#   acct 4: debit -75.25 (negative, non-zero)
#   acct 5: zero            -> 2525.00
# so the golden pins what the program really writes for non-zero inputs
# (OUT-ACCT-REC is not re-initialised between records).
# Outputs: work/synthetic-mixed-debit/{fixtures,ksds,CBACT01C}
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO="$(cd "$HERE/../.." && pwd)"
WORK="${WORK:-$HERE/work}"
SCEN="$WORK/synthetic-mixed-debit"
FIX="$SCEN/fixtures"
mkdir -p "$FIX"
python3 - "$REPO/app/data/ASCII" "$FIX" <<'PY'
import sys
sys.path.insert(0, sys.argv[0] if False else __import__("os").path.join(sys.argv[1], "..", "..", "..", "test-harness"))
from records import encode_zoned
data, fix = sys.argv[1:3]
recs = open(data + '/acctdata.txt').read().split('\n')[:5]
# ACCT-CURR-CYC-DEBIT is PIC S9(10)V99 at offset 90 (CVACT01Y)
def set_debit(rec, value):
    return rec[:90] + encode_zoned(value, 12, 2, True, False).decode('ascii') + rec[102:]
recs[0] = set_debit(recs[0], "10.00")
recs[2] = set_debit(recs[2], "120.50")
recs[3] = set_debit(recs[3], "-75.25")
assert all(len(r) == 300 for r in recs)
open(fix + '/acctdata.txt', 'w').write('\n'.join(recs) + '\n')
print('fixtures written to', fix)
PY
export WORK KSDS="$SCEN/ksds" OUT="$SCEN/CBACT01C"
ACCT_SRC="$FIX/acctdata.txt" "$HERE/load_ksds.sh"
"$HERE/run_cbact01c.sh"
