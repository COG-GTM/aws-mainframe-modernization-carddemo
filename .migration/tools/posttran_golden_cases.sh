#!/usr/bin/env bash
# Builds every POSTTRAN golden case with the GnuCOBOL oracle and copies inputs + expected outputs into DEST
# (default modernization/carddemo-app/src/test/resources/golden/posttran). Reads app/** only.
# Usage: .migration/tools/posttran_golden_cases.sh WORK_DIR [DEST]
#   WORK_DIR must not exist (scratch for oracle runs); DEST/<case> must not exist: nothing is ever deleted.
# Case layout: <case>/case.properties, <case>/input/DALYTRAN (the exact bytes CBTRN02C read: RECFM=FB 350, ISO-8859-1,
#   or the CP037 source file), optional <case>/input/{acctdata,cardxref,tcatbal}.txt overrides,
#   <case>/expected/{DALYREJS,SYSOUT,RETURN-CODE,TRANSACT.unl,ACCTDATA.unl,TCATBALF.unl} (oracle output, unchanged).
set -euo pipefail
REPO=$(cd "$(dirname "$0")/../.." && pwd)
WORK=$(realpath -m -- "${1:?usage: posttran_golden_cases.sh WORK_DIR [DEST]}")
DEST=$(realpath -m -- "${2:-$REPO/modernization/carddemo-app/src/test/resources/golden/posttran}")
ORACLE="$REPO/.migration/tools/posttran_gnucobol_oracle.sh"
export ORACLE_NOW=${ORACLE_NOW-2022/07/18 10:30:45}
[ -e "$WORK" ] && { echo "refusing WORK_DIR '$WORK': it must not exist" >&2; exit 2; }
mkdir -p "$WORK/inputs" "$DEST"

run_case() { # name encoding dalytran [masters_dir]
  local name=$1 enc=$2 daly=$3 masters=${4:-}
  [ -e "$DEST/$name" ] && { echo "refusing case '$name': $DEST/$name exists" >&2; exit 2; }
  local env=(DALYTRAN_ENCODING="$enc")
  if [ -n "$masters" ]; then
    for f in acctdata cardxref tcatbal; do
      [ -f "$masters/$f.txt" ] && env+=("$(echo "$f" | tr a-z A-Z)_FILE=$masters/$f.txt")
    done
  fi
  env "${env[@]}" "$ORACLE" "$WORK/$name" "$daly" > "$WORK/$name.log" 2>&1 || { cat "$WORK/$name.log" >&2; exit 1; }
  mkdir -p "$DEST/$name/input" "$DEST/$name/expected"
  if [ "$enc" = CP037 ]; then cp "$daly" "$DEST/$name/input/DALYTRAN"; else cp "$WORK/$name/data/DALYTRAN" "$DEST/$name/input/DALYTRAN"; fi
  [ -n "$masters" ] && for f in acctdata cardxref tcatbal; do [ -f "$masters/$f.txt" ] && cp "$masters/$f.txt" "$DEST/$name/input/"; done
  cp "$WORK/$name/expected/"* "$DEST/$name/expected/"
  local charset=ISO-8859-1; [ "$enc" = CP037 ] && charset=IBM037
  printf 'dalytran.charset=%s\noracle.now=%s\n' "$charset" "$ORACLE_NOW" > "$DEST/$name/case.properties"
  echo "$name RC=$(cat "$DEST/$name/expected/RETURN-CODE") rejects=$(( $(wc -c < "$DEST/$name/expected/DALYREJS") / 430 ))"
}

run_case baseline ASCII "$REPO/app/data/ASCII/dailytran.txt"
python3 "$REPO/.migration/tools/posttran_synthetic_cases.py" "$DEST/baseline/expected/DALYREJS" "$WORK/inputs/syn"
run_case syn-all ASCII "$WORK/inputs/syn/syn_all.txt"
run_case syn-ok ASCII "$WORK/inputs/syn/syn_ok.txt"
: > "$WORK/inputs/empty.txt"
run_case empty ASCII "$WORK/inputs/empty.txt"
run_case cp037 CP037 "$REPO/app/data/EBCDIC/AWS.M2.CARDDEMO.DALYTRAN.PS"

# acct-missing (FR-POSTTRAN-03): drop from ACCTDATA the account of the first baseline-posted card; post 1 record of
# another account, then 2 records of the dropped account's card -> 2 x 0101 ACCOUNT RECORD NOT FOUND.
mkdir -p "$WORK/inputs/acct-missing"
python3 - "$REPO" "$DEST/baseline/expected/DALYREJS" "$WORK/inputs/acct-missing" <<'PY'
import sys
repo, rej_path, out = sys.argv[1:]
daly = [l.rstrip('\r\n') for l in open(f'{repo}/app/data/ASCII/dailytran.txt', encoding='latin-1')]
xref = {l[:16]: l[25:36] for l in (x.rstrip('\r\n') for x in open(f'{repo}/app/data/ASCII/cardxref.txt', encoding='latin-1'))}
rej = open(rej_path, 'rb').read()
rejected = {rej[i:i + 16].decode('latin-1') for i in range(0, len(rej), 430)}
ok = [l for l in daly if l[:16] not in rejected]
victim = xref[ok[0][262:278]]
mine = [l for l in daly if xref.get(l[262:278]) == victim][:2]
other = next(l for l in ok if xref[l[262:278]] != victim)
assert len(mine) == 2, 'need two DALYTRAN records for the dropped account'
open(f'{out}/dailytran.txt', 'w', encoding='latin-1').write('\n'.join([other] + mine) + '\n')
accts = [l for l in open(f'{repo}/app/data/ASCII/acctdata.txt', encoding='latin-1')]
kept = [l for l in accts if l[:11] != victim]
assert len(kept) == len(accts) - 1
open(f'{out}/acctdata.txt', 'w', encoding='latin-1').write(''.join(kept))
print(f'acct-missing: dropped account {victim}')
PY
run_case acct-missing ASCII "$WORK/inputs/acct-missing/dailytran.txt" "$WORK/inputs/acct-missing"

# dup-id (FR-POSTTRAN-13, R-06): records 1, 2, 1 -> duplicate TRAN-ID on TRANSACT write -> status 22 -> abend U999 (RC 231)
head -2 "$REPO/app/data/ASCII/dailytran.txt" > "$WORK/inputs/dup.txt"; head -1 "$REPO/app/data/ASCII/dailytran.txt" >> "$WORK/inputs/dup.txt"
run_case dup-id ASCII "$WORK/inputs/dup.txt"

# dup-id table expectation under per-record atomic posting (D-026, D-042): CBTRN02C rewrites TCATBAL and ACCOUNT for
# record 3 before its TRANSACT WRITE fails, so the oracle tables keep that half-posted record; the Java job rolls the
# record back. The oracle run on the records before the abending one gives the atomic end state of the three tables.
head -2 "$REPO/app/data/ASCII/dailytran.txt" > "$WORK/inputs/dup-atomic.txt"
"$ORACLE" "$WORK/dup-id-atomic" "$WORK/inputs/dup-atomic.txt" > "$WORK/dup-id-atomic.log" 2>&1 || { cat "$WORK/dup-id-atomic.log" >&2; exit 1; }
mkdir -p "$DEST/dup-id/expected-atomic"
cp "$WORK/dup-id-atomic/expected/"{TRANSACT,ACCTDATA,TCATBALF}.unl "$DEST/dup-id/expected-atomic/"
echo 'tables.expected=expected-atomic' >> "$DEST/dup-id/case.properties"
echo "dup-id atomic tables: RC=$(cat "$WORK/dup-id-atomic/expected/RETURN-CODE") (records 1-2 only)"
