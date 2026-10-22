#!/usr/bin/env python3
"""Builds fixtures/ for the CBACT04C interest golden run (rates, signs and
overflow cases the seed data does not exercise). Run from this directory."""
from decimal import Decimal

POS = '{ABCDEFGHI'
NEG = '}JKLMNOPQR'
SEED = '../../../../app/data/ASCII/'


def zoned(v, digits, scale):
    d = Decimal(v)
    s = str(int(abs(d) * (10 ** scale))).rjust(digits, '0')
    return s[:-1] + (NEG if d < 0 else POS)[int(s[-1])]


acct = {l[:11]: l.rstrip('\n').ljust(300) for l in open(SEED + 'acctdata.txt')}


def setacct(i, group, bal=None, cyc_credit=None, cyc_debit=None):
    k = str(i).rjust(11, '0')
    r = list(acct[k])
    r[112:122] = group.ljust(10)
    if bal is not None:
        r[12:24] = zoned(bal, 12, 2)
    if cyc_credit is not None:
        r[78:90] = zoned(cyc_credit, 12, 2)
    if cyc_debit is not None:
        r[90:102] = zoned(cyc_debit, 12, 2)
    acct[k] = ''.join(r)


setacct(1, 'A000000000', cyc_credit='125.50', cyc_debit='-40.25')
setacct(2, 'NOGROUP')
setacct(3, 'ODDRATE', bal='9999999990.00')
setacct(4, 'ZEROAPR', cyc_credit='1.00')
setacct(5, 'ODDRATE', bal='-0.01')
with open('fixtures/acctdata-interest.txt', 'w') as f:
    f.writelines(acct[k].rstrip() + '\n' for k in sorted(acct))

disc = [l.rstrip('\n') for l in open(SEED + 'discgrp.txt')]
odd = [('01', '0001', '17.99'), ('01', '0002', '-3.33'), ('01', '0003', '9999.99'),
       ('01', '0004', '0.01'), ('02', '0001', '-9999.99')]
disc += ['ODDRATE'.ljust(10) + t + c + zoned(r, 6, 2) for t, c, r in odd]
with open('fixtures/discgrp-interest.txt', 'w') as f:
    f.writelines(l.rstrip() + '\n' for l in sorted(disc))

tc = [(1, '01', '0001', '1234.56'), (1, '01', '0002', '-500.01'), (1, '02', '0001', '777.77'),
      (1, '04', '0003', '0.08'),
      (2, '01', '0003', '999999999.99'), (2, '01', '0004', '-1.00'), (2, '07', '0001', '55.55'),
      (3, '01', '0001', '0.07'), (3, '01', '0002', '333.33'), (3, '01', '0003', '999999999.99'),
      (3, '01', '0004', '-999999999.99'), (3, '02', '0001', '999999999.99'),
      (4, '01', '0001', '100.00'),
      (5, '01', '0002', '0.01'), (5, '01', '0003', '-12345.67')]
with open('fixtures/tcatbal-interest.txt', 'w') as f:
    f.writelines(str(a).rjust(11, '0') + t + c + zoned(b, 11, 2) + '\n' for a, t, c, b in sorted(tc))
