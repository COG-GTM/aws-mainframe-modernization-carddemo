#!/usr/bin/env bash
# POSTTRAN end-to-end inputs, built from app/data only (never modifies it). Used by posttran_e2e_oracle.sh and posttran_e2e.sh.
# Usage: .migration/tools/posttran_e2e_inputs.sh OUT_DIR
# Writes OUT_DIR/{dailytran.txt, DALYTRAN, DALYTRAN.dup151, DALYTRAN.first150, dup151.txt, first150.txt, empty.txt, DALYTRAN.empty}
#   DALYTRAN           = app/data/ASCII/dailytran.txt as RECFM=FB 350-byte ISO-8859-1 records (no newline)
#   DALYTRAN.dup151    = same, record 151 (a posting record) carrying record 1's TRAN-ID -> duplicate key on TRANSACT
#   DALYTRAN.first150  = records 1-150 (the atomic end state of the dup151 abend, D-042)
set -euo pipefail
REPO=$(cd "$(dirname "$0")/../.." && pwd)
OUT=$(realpath -m -- "${1:?OUT_DIR}")
mkdir -p "$OUT"
pad() { tr -d '\r' < "$1" | awk '{printf "%-350s\n", $0}'; }
pad "$REPO/app/data/ASCII/dailytran.txt" > "$OUT/dailytran.txt"
awk 'NR==1 {id=substr($0,1,16)} NR==151 {$0=id substr($0,17)} {print}' "$OUT/dailytran.txt" > "$OUT/dup151.txt"
head -150 "$OUT/dailytran.txt" > "$OUT/first150.txt"
: > "$OUT/empty.txt"
for f in dailytran dup151 first150 empty; do
  tr -d '\n' < "$OUT/$f.txt" > "$OUT/DALYTRAN.$f"
done
mv "$OUT/DALYTRAN.dailytran" "$OUT/DALYTRAN"
cp "$REPO/app/data/EBCDIC/AWS.M2.CARDDEMO.DALYTRAN.PS" "$OUT/DALYTRAN.cp037"
wc -c "$OUT"/DALYTRAN*
