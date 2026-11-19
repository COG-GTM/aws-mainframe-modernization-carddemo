#!/usr/bin/env python3
"""Build synthetic POSTTRAN golden inputs from app/data/ASCII/dailytran.txt (read-only).

Usage: posttran_synthetic_cases.py <golden DALYREJS from R-04> <out_dir>
Writes <out_dir>/syn_all.txt (6 records: post, 100, 103, 102+103 -> 103, negative amount,
new TCATBAL key) and <out_dir>/syn_ok.txt (2 postable records, RC 0). Offsets from app/cpy/CVTRA06Y.cpy.
"""
import os, sys
rej_path, out = sys.argv[1], sys.argv[2]
os.makedirs(out, exist_ok=True)
L = [l.rstrip('\n') for l in open('app/data/ASCII/dailytran.txt')]
rej = open(rej_path, 'rb').read()
rids = {rej[i:i + 16].decode() for i in range(0, len(rej), 430)}
ok = [l for l in L if l[:16] not in rids]
bad = [l for l in L if l[:16] in rids]
def s(l, a, b, v): return l[:a] + v + l[b:]
def nid(l, n): return s(l, 0, 16, 'SYN%013d' % n)
p, o = ok[0], bad[0]
r = [nid(p, 1),                                        # posts
     s(nid(p, 2), 262, 278, '9999999999999999'),       # DALYTRAN-CARD-NUM unknown -> 100
     s(nid(p, 3), 278, 288, '2099-01-01'),             # ORIG-TS after expiry -> 103
     s(nid(o, 4), 278, 288, '2099-01-01'),             # overlimit AND expired -> 103 overwrites 102
     s(nid(p, 5), 132, 143, p[132:142] + '}'),         # DALYTRAN-AMT negative -> CYC-DEBIT
     s(nid(p, 6), 18, 22, '0099')]                     # DALYTRAN-CAT-CD with no TCATBAL row -> create
open(os.path.join(out, 'syn_all.txt'), 'w').write('\n'.join(r) + '\n')
open(os.path.join(out, 'syn_ok.txt'), 'w').write('\n'.join([r[0], r[4]]) + '\n')
