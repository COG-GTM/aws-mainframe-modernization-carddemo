#!/usr/bin/env python3
"""Reproduce CREASTMT STEP010 (DFSORT) on an ASCII TRANSACT-layout file.

SORT FIELDS=(263,16,CH,A,1,16,CH,A)
OUTREC FIELDS=(1:263,16,17:1,262,279:279,50)
The OUTREC builds 328 bytes; the 350-byte LRECL is filled with spaces.
"""
import sys

src, dst = sys.argv[1], sys.argv[2]
with open(src, "r", encoding="ascii", newline="") as f:
    recs = [line.rstrip("\r\n").ljust(350)[:350] for line in f if line.strip("\r\n")]
recs.sort(key=lambda r: (r[262:278], r[0:16]))
with open(dst, "w", encoding="ascii", newline="\n") as f:
    for r in recs:
        out = (r[262:278] + r[0:262] + r[278:328]).ljust(350)
        f.write(out + "\n")
