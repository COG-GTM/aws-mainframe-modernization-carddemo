#!/usr/bin/env python3
"""Independent CBTRN02C parity inputs (Wave 1 parity pass). Reads app/data/ASCII only.

Usage: cbtrn02c_parity_cases.py <out_dir>
Writes <out_dir>/<case>/{dailytran.txt[,acctdata.txt,cardxref.txt,tcatbal.txt]} for every case below. Master files are
the sample masters with targeted field changes on real account keys (mock-data cases, playbook step 3). Expected
results are NOT computed here: every case is run through GnuCOBOL by cbtrn02c_parity_oracle.sh.
Offsets: CVTRA06Y (DALYTRAN 350), CVACT01Y (ACCOUNT 300), CVTRA01Y (TCATBAL 50), CVACT03Y (XREF 50).
"""
import os
import sys
from decimal import Decimal

REPO = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..'))
OUT = sys.argv[1]


def lines(name):
    return [l.rstrip('\r\n') for l in open(f'{REPO}/app/data/ASCII/{name}', encoding='latin-1')]


DALY0 = lines('dailytran.txt')[0].ljust(350)
ACCT = {l[:11]: l.ljust(300) for l in lines('acctdata.txt')}
XREF = lines('cardxref.txt')
TCAT = lines('tcatbal.txt')
CARD_OF = {}
for x in XREF:
    CARD_OF.setdefault(int(x[25:36]), x[:16])

POS = '{ABCDEFGHI'
NEG = '}JKLMNOPQR'


def zoned(value, digits, scale=2):
    """PIC S9(digits-scale)V9(scale) DISPLAY with trailing overpunch (sample-file convention, -fsign=EBCDIC)."""
    v = Decimal(value)
    n = int(abs(v) * (10 ** scale))
    s = str(n).zfill(digits)
    assert len(s) == digits, (value, digits)
    last = (NEG if v < 0 or str(value).startswith('-') else POS)[int(s[-1])]
    return s[:-1] + last


def put(rec, off, val):
    return rec[:off] + val + rec[off + len(val):]


def tran(tid, acct=None, amt='10.00', card=None, type_cd='01', cat='0001', orig='2022-06-10 19:27:53.000000', raw_amt=None):
    r = DALY0
    r = put(r, 0, tid.ljust(16)[:16])
    r = put(r, 16, type_cd)
    r = put(r, 18, cat)
    r = put(r, 132, raw_amt if raw_amt else zoned(amt, 11))
    r = put(r, 262, card if card else CARD_OF[acct])
    r = put(r, 278, orig.ljust(26)[:26])
    return r


def acct(n, **f):
    """Copy of sample account n with fields changed: bal, limit, credit, debit (decimal strings), expiry (text)."""
    r = ACCT['%011d' % n]
    for k, off in (('bal', 12), ('limit', 24), ('credit', 78), ('debit', 90)):
        if k in f:
            r = put(r, off, zoned(f[k], 12))
    if 'expiry' in f:
        r = put(r, 58, f['expiry'].ljust(10)[:10])
    return r


def write(case, daly, accts=None, tcat=None, drop_accts=()):
    d = os.path.join(OUT, case)
    os.makedirs(d, exist_ok=True)
    open(f'{d}/dailytran.txt', 'w', encoding='latin-1').write(''.join(l.rstrip() + '\n' for l in daly))
    if accts or drop_accts:
        m = dict(ACCT)
        for r in accts or []:
            m[r[:11]] = r
        for n in drop_accts:
            del m['%011d' % n]
        open(f'{d}/acctdata.txt', 'w', encoding='latin-1').write(''.join(m[k].rstrip() + '\n' for k in sorted(m)))
    if tcat:
        t = {l[:17]: l for l in TCAT}
        t.update({l[:17]: l for l in tcat})
        open(f'{d}/tcatbal.txt', 'w', encoding='latin-1').write(''.join(t[k] + '\n' for k in sorted(t)))


def tcat(n, type_cd, cat, bal):
    return '%011d%s%s%s%s' % (n, type_cd, cat, zoned(bal, 11), '0' * 22)


EXP = dict(expiry='2030-12-31')  # far expiry so only the rule under test can reject

# limit-exact (PR-06 / FR-04): CREDIT-LIMIT >= CYC-CREDIT - CYC-DEBIT + AMT, debit holds negative sums (cbl:403-407)
write('limit-exact', [
    tran('LIMEXACT00000001', 1, '500.00'),    # 300 - (-200) + 500 = 1000.00 = limit -> posts
    tran('LIMEXACT00000002', 2, '500.01'),    # 1000.01 > 1000.00 -> 0102
    tran('LIMEXACT00000003', 3, '-0.01'),     # 999.99 - 0 - 0.01 = 999.98 = limit -> posts (negative amount)
    tran('LIMEXACT00000004', 4, '0.00'),      # 1000.00 + 0.00 = 1000.00 = limit -> posts (zero amount)
], accts=[acct(1, limit='1000.00', credit='300.00', debit='-200.00', **EXP),
          acct(2, limit='1000.00', credit='300.00', debit='-200.00', **EXP),
          acct(3, limit='999.98', credit='999.99', debit='0.00', **EXP),
          acct(4, limit='1000.00', credit='1000.00', debit='0.00', **EXP)])

# expiry-boundary (PR-07 / FR-05): text compare ACCT-EXPIRAION-DATE >= DALYTRAN-ORIG-TS(1:10) (cbl:414)
write('expiry-boundary', [
    tran('EXPBOUND00000001', 5, orig='2022-06-10 19:27:53.000000'),   # expiry 2022-06-10 = orig date -> posts
    tran('EXPBOUND00000002', 6, orig='2022-06-10 19:27:53.000000'),   # expiry 2022-06-09 -> 0103
    tran('EXPBOUND00000003', 7, orig='2022-06-10Z99:99:99.999999'),   # only (1:10) compared -> posts
    tran('EXPBOUND00000004', 8, orig='2022-06-10 19:27:53.000000'),   # expiry spaces < digits -> 0103
    tran('EXPBOUND00000005', 9, orig=''),                             # ORIG-TS spaces <= any expiry -> posts
    tran('EXPBOUND00000006', 10, orig='2022-06-10 19:27:53.000000'),  # expiry '2022/01/01': '/' > '-' as text -> posts
    tran('EXPBOUND00000007', 11, orig='2022-06-11 00:00:00.000000'),  # expiry 2022-06-10, orig next day -> 0103
], accts=[acct(5, expiry='2022-06-10'), acct(6, expiry='2022-06-09'), acct(7, expiry='2022-06-10'),
          acct(8, expiry=''), acct(9, expiry='2022-06-10'), acct(10, expiry='2022/01/01'),
          acct(11, expiry='2022-06-10')])

# reason-precedence (PR-03/04/05/06/07, FR-02..06): each reason, 103 over 102, trailer reset between rejects
write('reason-precedence', [
    tran('REASONS000000001', card='4000000000000001'),              # 0100 unknown card
    tran('REASONS000000002', 12, '9000.00'),                        # 0102 only
    tran('REASONS000000003', 13),                                   # 0101 xref -> dropped account 13
    tran('REASONS000000004', 14, '9000.00'),                        # 0103 over 0102 (expired AND over limit)
    tran('REASONS000000005', 15),                                   # 0103 only
    tran('REASONS000000006', card='4000000000000001'),              # 0100 again after a 0103 (desc fully replaced)
    tran('REASONS000000007', 16),                                   # posts
], accts=[acct(12, limit='100.00', credit='0.00', debit='0.00', **EXP),
          acct(14, limit='100.00', credit='0.00', debit='0.00', expiry='2020-01-01'),
          acct(15, expiry='2020-01-01'), acct(16, **EXP)], drop_accts=[13])

# same-account-multi (PR-06/10/11, FR-04/07/08): validation re-reads the account updated by the previous posting
write('same-account-multi', [
    tran('SAMEACCT00000001', 17, '40.00'),    # temp 40 -> posts; credit 40
    tran('SAMEACCT00000002', 17, '40.00'),    # temp 80 -> posts; credit 80
    tran('SAMEACCT00000003', 17, '40.00'),    # temp 120 > 100 -> 0102
    tran('SAMEACCT00000004', 17, '-50.00'),   # temp 30 -> posts; debit -50, bal +30
    tran('SAMEACCT00000005', 17, '0.00'),     # temp 80 - (-50) + 0 = 130 > 100 -> 0102 (zero amount rejected)
    tran('SAMEACCT00000006', 17, '-200.00'),  # temp 80 + 50 - 200 = -70 -> posts; debit -250
    tran('SAMEACCT00000007', 17, '10.00', cat='0007'),  # temp 80 + 250 + 10 = 340 > 100 -> 0102
], accts=[acct(17, limit='100.00', bal='0.00', credit='0.00', debit='0.00', **EXP)])

# tcatbal-create-vs-rewrite (PR-10, FR-07): first TCATBAL access of the run is a create (SYSOUT line), then rewrite
write('tcatbal-create-rewrite', [
    tran('TCATBAL000000001', 18, '11.11', cat='0005'),               # new key 18/01/0005 -> create + SYSOUT
    tran('TCATBAL000000002', 18, '22.22', cat='0005'),               # same key -> rewrite (no SYSOUT line)
    tran('TCATBAL000000003', 18, '33.33'),                           # existing sample key 18/01/0001 -> rewrite
    tran('TCATBAL000000004', 18, '-4.44', type_cd='02', cat='0001'), # new key 18/02/0001 after a successful read
    tran('TCATBAL000000005', 19, '5.55', type_cd='03', cat='0002'),  # key seeded with balance 123.45 -> rewrite
], accts=[acct(18, **EXP), acct(19, **EXP)], tcat=[tcat(19, '03', '0002', '123.45')])

# overflow (D-047): COMPUTE/ADD without ON SIZE ERROR (cbl:403-405, 508, 527, 547-551)
write('ovf-temp-bal', [
    tran('OVFTEMP000000001', 20, '5.00'),     # temp = 1000000000.00 + 5.00 overflows S9(09)V99
    tran('OVFTEMP000000002', 21, '0.02'),     # temp = 999999999.99 + 0.02 overflows
    tran('OVFTEMP000000003', 22, '1.00'),     # temp = 0 - (-999999999.99) + 1.00 overflows through the debit
], accts=[acct(20, limit='1000.00', credit='1000000000.00', debit='0.00', **EXP),
          acct(21, limit='1000.00', credit='999999999.99', debit='0.00', **EXP),
          acct(22, limit='1000.00', credit='0.00', debit='-999999999.99', **EXP)])
write('ovf-acct-bal', [
    tran('OVFBAL0000000001', 23, '1.00'),     # CURR-BAL 9999999999.99 + 1.00 overflows S9(10)V99
    tran('OVFBAL0000000002', 24, '-1.00'),    # CURR-BAL -9999999999.99 - 1.00 overflows
], accts=[acct(23, bal='9999999999.99', limit='1000.00', credit='0.00', debit='0.00', **EXP),
          acct(24, bal='-9999999999.99', limit='1000.00', credit='0.00', debit='0.00', **EXP)])
write('ovf-cyc', [
    tran('OVFCYC0000000001', 25, '1.00'),     # CYC-CREDIT 9999999999.50 + 1.00 overflows; temp overflows too
    tran('OVFCYC0000000002', 26, '-1.00'),    # CYC-DEBIT -9999999999.50 - 1.00 overflows; temp overflows too
], accts=[acct(25, limit='9999999999.99', credit='9999999999.50', debit='0.00', **EXP),
          acct(26, limit='9999999999.99', credit='0.00', debit='-9999999999.50', **EXP)])
write('ovf-tcatbal', [
    tran('OVFTCAT000000001', 27, '1.00'),                 # TRAN-CAT-BAL 999999999.99 + 1.00 overflows S9(09)V99
    tran('OVFTCAT000000002', 28, '-0.01'),                # -999999999.99 - 0.01 overflows
    tran('OVFTCAT000000003', 29, '999999999.99', cat='0009'),  # create with max amount (fits)
], accts=[acct(27, **EXP), acct(28, **EXP),
          acct(29, limit='9999999999.99', credit='0.00', debit='0.00', **EXP)],
   tcat=[tcat(27, '01', '0001', '999999999.99'), tcat(28, '01', '0001', '-999999999.99')])

# negative zero amount (zoned '}' sign): IF DALYTRAN-AMT >= 0 (cbl:548) and value of -0.00
write('negative-zero', [
    tran('NEGZERO000000001', 30, raw_amt='0000000000}'),
], accts=[acct(30, **EXP)])

# negative-zero-balances (D-047/D-052): -0 produced by truncation and by exact-zero ADDs on every signed balance field
write('negative-zero-balances', [
    tran('NEGZBAL000000001', 38, '-0.01'),    # CURR-BAL -9999999999.99 - 0.01 truncates to -0 (cbl:547)
    tran('NEGZBAL000000002', 39, '-5.00'),    # CURR-BAL 5.00 - 5.00 and TCATBAL 5.00 - 5.00: exact zero (cbl:527, 547)
    tran('NEGZBAL000000003', 40, '-0.01'),    # CYC-DEBIT -9999999999.99 - 0.01 truncates to -0 (cbl:551)
    tran('NEGZBAL000000004', 41, '5.00'),     # CYC-CREDIT -5.00 + 5.00 and CURR-BAL -5.00 + 5.00: exact zero (cbl:547, 549)
    tran('NEGZBAL000000005', 42, '0.00'),     # CURR-BAL -0.00 + 0.00, TCATBAL -0.00 + 0.00 (seeded negative zero)
], accts=[acct(38, bal='-9999999999.99', **EXP),
          acct(39, bal='5.00', **EXP),
          acct(40, limit='9999999999.99', credit='0.00', debit='-9999999999.99', **EXP),
          acct(41, bal='-5.00', credit='-5.00', debit='0.00', **EXP),
          acct(42, bal='-0.00', **EXP)],
   tcat=[tcat(39, '01', '0001', '5.00'), tcat(42, '01', '0001', '-0.00')])

# dup-rejected-id (PR-12): a duplicate TRAN-ID only abends when the duplicate is posted (TRANSACT WRITE, cbl:564)
write('dup-rejected-id', [
    tran('DUPREJ0000000001', 31),                              # posts
    tran('DUPREJ0000000001', card='4000000000000001'),         # same id, 0100 -> rejected, no WRITE, no abend
    tran('DUPREJ0000000002', card='4000000000000001'),         # 0100
    tran('DUPREJ0000000002', 32),                              # same id as a rejected record -> posts
], accts=[acct(31, **EXP), acct(32, **EXP)])

# dup-abend-newkey (PR-10/11/12/17, FR-13, D-042): the abending record creates a TCATBAL key and rewrites the account
# before the TRANSACT WRITE fails; later records are never read
DUPAB = [
    tran('DUPABND000000001', 33, '10.00'),
    tran('DUPABND000000002', card='4000000000000001'),          # 0100 reject before the abend
    tran('DUPABND000000001', 34, '20.00', cat='0006'),          # dup id: TCATBAL 34/01/0006 created, account 34 rewritten
    tran('DUPABND000000003', 35, '30.00'),                      # never processed
]
write('dup-abend-newkey', DUPAB, accts=[acct(33, **EXP), acct(34, **EXP), acct(35, **EXP)])
write('dup-abend-newkey.atomic', DUPAB[:2], accts=[acct(33, **EXP), acct(34, **EXP), acct(35, **EXP)])

# restart-fixed (D-012, FR-11): first leg abends on the dup id; operator replaces record 3 with a new id and restarts
FIXED = list(DUPAB)
FIXED[2] = tran('DUPABND000000009', 34, '20.00', cat='0006')
write('restart-fixed', FIXED, accts=[acct(33, **EXP), acct(34, **EXP), acct(35, **EXP)])

# rerun (D-024, FR-11): same DALYTRAN posted by two consecutive new instances on the same tables
write('rerun', [
    tran('RERUN00000000001', 36, '60.00'),    # run 1 posts (temp 60); run 2 temp 120 > 100 -> 0102
    tran('RERUN00000000002', 37, '10.00'),    # posts both runs
    tran('RERUN00000000003', card='4000000000000001'),  # 0100 both runs
], accts=[acct(36, limit='100.00', credit='0.00', debit='0.00', **EXP), acct(37, **EXP)])
print('cases written to', OUT)
