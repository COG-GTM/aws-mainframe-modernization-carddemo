"""Synthetic fixed-record fixtures and an independent integer oracle."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import random
import shutil
import subprocess

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[2]
BUILD = HERE / '.build'
EXPECTED = HERE / 'expected'
GENERATED = BUILD / 'generated'
CLOCK = '2022-07-18 12:34:56'
SPECS = {'ACCTFILE': (300, 11), 'XREFFILE': (50, 16),
         'TCATBALF': (50, 17), 'DISCGRP': (50, 16), 'TRANFILE': (350, 16)}


def signed(n, width):
    s = f'{abs(n):0{width}d}'
    return s[:-1] + ('{ABCDEFGHI' if n >= 0 else '}JKLMNOPQR')[int(s[-1])]


def number(s):
    last = s[-1:]
    for chars, sign in [(b'{ABCDEFGHI', 1), (b'}JKLMNOPQR', -1)]:
        if last in [bytes([c]) for c in chars]:
            return sign * int(s[:-1] + str(chars.index(last)).encode())
    raise AssertionError(s)


def account(i, limit=10000000, expiry='2030-01-01'):
    return (f'{i:011d}Y' + signed(0, 12) + signed(limit, 12)
            + signed(0, 12) + '2020-01-01' + expiry + '2020-01-01'
            + signed(0, 12) * 2 + '00000     ' + 'SYNTH     ' + ' ' * 178).encode()


def transaction(name, i, amount, cat=1):
    return (f'{name:<16}01{cat:04d}' + 'SYNTH     ' + f'{name:<100}'
            + signed(amount, 11) + '000000000' + ' ' * 110
            + f'{i:016d}' + '2022-07-18-00.00.00.000000' * 2 + ' ' * 20).encode()


def category(i, cat, amount):
    return (f'{i:011d}01{cat:04d}' + signed(amount, 11) + ' ' * 22).encode()


def write_records(path, records):
    path.write_bytes(b''.join(records))


def records(path, size):
    data = path.read_bytes()
    assert len(data) % size == 0, path
    return [data[i:i + size] for i in range(0, len(data), size)]


def run(args, **kwargs):
    return subprocess.run(list(map(str, args)), check=True, **kwargs)


def bridge(name, size, key):
    alternate = 'alternate record key is alt-key' if name == 'XREFFILE' else ''
    layout = ('05 prime-key pic x(16).\n05 filler pic x(9).\n'
              '05 alt-key pic x(11).\n05 filler pic x(14).'
              if alternate else f'05 prime-key pic x({key}).\n05 filler pic x({size-key}).')
    source = f'''       >>source format free
identification division.
program-id. bridge.
environment division.
input-output section.
file-control.
 select idx assign to IDXDATA organization indexed
 access sequential record key prime-key {alternate} file status fs.
 select flat assign to FLATDATA organization sequential.
data division.
file section.
fd idx.
01 row-data.
{layout}
fd flat.
01 flat-data pic x({size}).
working-storage section.
01 fs pic xx.
01 mode-value pic x(4).
01 done pic 9 value 0.
procedure division.
 accept mode-value from command-line
 if mode-value = 'load'
   open input flat output idx
 else
   open input idx output flat
 end-if
 if fs not = '00' display 'OPEN: ' fs end-display
   move 1 to return-code goback end-if
 perform until done = 1
   if mode-value = 'load'
     read flat at end move 1 to done not at end
       move flat-data to row-data write row-data
       if fs not = '00' display 'WRITE: ' fs end-display
         move 1 to return-code goback end-if
     end-read
   else
     read idx at end move 1 to done not at end
       move row-data to flat-data write flat-data end-read
   end-if
 end-perform
 close idx flat
 move 0 to return-code
 goback.
'''
    path = BUILD / f'{name}.cbl'
    path.write_text(source)
    run(['cobc', '-x', '-free', '-o', BUILD / name, path])


def transfer(name, mode, work, flat):
    env = dict(os.environ, **{'DD_IDXDATA': str(work / name),
                             'DD_FLATDATA': str(flat)})
    run([BUILD / name, mode], env=env)


def fixture(volume):
    count = 3000 if volume else 4
    accounts = [account(i) for i in range(1, count + 1)]
    xrefs = [(f'{i:016d}{i:09d}{i:011d}' + ' ' * 14).encode()
             for i in range(1, count + 1)]
    cats = [category(1, 1, 10001), category(1, 2, -10001), category(1, 3, 0)]
    if volume:
        rng = random.Random(104)
        daily = [transaction(f'V{i:015d}', i, rng.randrange(-9999, 10000))
                 for i in range(1, count + 1)]
    else:
        accounts[1] = account(2, limit=100)
        accounts[2] = account(3, expiry='2022-07-17')
        xrefs.append((f'{999:016d}{999:09d}{999:011d}' + ' ' * 14).encode())
        daily = [transaction('CLEAN_EXISTING', 1, 100),
                 transaction('CLEAN_NEW', 4, 10001),
                 transaction('XREF_MISS', 998, 100),
                 transaction('NO_ACCOUNT', 999, 100),
                 transaction('OVER_LIMIT', 2, 101),
                 transaction('AFTER_EXPIRY', 3, 100),
                 transaction('EXPIRY_OVERRIDES', 3, 10000001)]
    rates = [(f'{"DEFAULT":10}01{cat:04d}' + signed(1250, 6) + ' ' * 28).encode()
             for cat in (1, 2, 3)]
    return {'ACCTFILE': accounts, 'XREFFILE': xrefs, 'TCATBALF': cats,
            'DISCGRP': rates, 'TRANFILE': [transaction('PRIOR_SEED', 1, 7)]}, daily


def execute(case, volume):
    work = BUILD / case
    out = GENERATED / case
    work.mkdir()
    out.mkdir()
    initial, daily = fixture(volume)
    for name, rows in initial.items():
        flat = out / f'initial.{name}.bin'
        write_records(flat, rows)
        transfer(name, 'load', work, flat)
    write_records(out / 'DALYTRAN.bin', daily)
    env = dict(os.environ)
    for name in SPECS:
        env[f'DD_{name}'] = str(work / name)
    env.update(DD_DALYTRAN=str(out / 'DALYTRAN.bin'),
               DD_DALYREJS=str(out / 'DALYREJS.bin'),
               DD_TRANSACT=str(out / 'SYSTRAN.bin'))
    codes = {}
    for program in ('CBTRN02C', 'CBACT04C'):
        result = subprocess.run(['faketime', '-f', CLOCK, str(BUILD / program)],
                                env=env, capture_output=True)
        (out / f'{program}.sysout.txt').write_bytes(result.stdout)
        (out / f'{program}.stderr.txt').write_bytes(result.stderr)
        codes[program] = result.returncode
        assert result.returncode == (0 if volume or program == 'CBACT04C' else 4), codes
        assert not result.stderr, result.stderr
        for name in SPECS:
            transfer(name, 'dump', work, out / f'{program}.{name}.bin')
    (out / 'return-codes.json').write_text(json.dumps(codes, indent=2) + '\n')
    verify(out, initial, daily, volume)
    print(f'{case}: {codes}; verified {len(daily)} daily records')


def verify(out, initial, daily, volume):
    rejects = records(out / 'DALYREJS.bin', 430)
    reasons = {r[:16].decode().strip(): int(r[350:354]) for r in rejects}
    assert reasons == ({} if volume else {'XREF_MISS': 100, 'NO_ACCOUNT': 101,
                      'OVER_LIMIT': 102, 'AFTER_EXPIRY': 103, 'EXPIRY_OVERRIDES': 103})
    accepted = [r for r in daily if r[:16].decode().strip() not in reasons]
    posted = records(out / 'CBTRN02C.TRANFILE.bin', 350)
    assert len(posted) == len(accepted)
    assert initial['TRANFILE'][0] not in posted
    by_id = {r[:16]: r for r in posted}
    for r in accepted:
        assert by_id[r[:16]][:304] == r[:304]
        assert by_id[r[:16]][304:330] == b'2022-07-18-12.34.56.000000'
    balances = {r[:17]: number(r[17:28]) for r in initial['TCATBALF']}
    currents = {r[:11]: 0 for r in initial['ACCTFILE']}
    credits = dict(currents)
    debits = dict(currents)
    for r in accepted:
        acct = f'{int(r[262:278]):011d}'.encode()
        key = acct + r[16:22]
        amount = number(r[132:143])
        balances[key] = balances.get(key, 0) + amount
        currents[acct] += amount
        (credits if amount >= 0 else debits)[acct] += amount
    actual = records(out / 'CBTRN02C.TCATBALF.bin', 50)
    assert {r[:17]: number(r[17:28]) for r in actual} == balances
    for r in records(out / 'CBTRN02C.ACCTFILE.bin', 300):
        assert number(r[12:24]) == currents[r[:11]]
        assert number(r[78:90]) == credits[r[:11]]
        assert number(r[90:102]) == debits[r[:11]]
    interest = records(out / 'SYSTRAN.bin', 350)
    assert len(interest) == len(balances)
    totals = dict.fromkeys(currents, 0)
    for n, ((key, bal), r) in enumerate(zip(sorted(balances.items()), interest), 1):
        cents = (abs(bal) * 1250 // 120000) * (-1 if bal < 0 else 1)
        assert number(r[132:143]) == cents
        assert r[:16] == f'2022071800{n:06d}'.encode()
        assert int(r[262:278]) == int(key[:11])
        assert r[278:330] == b'2022-07-18-12.34.56.000000' * 2
        totals[key[:11]] += cents
    last_account = max(balances)[:11]
    for r in records(out / 'CBACT04C.ACCTFILE.bin', 300):
        key = r[:11]
        assert number(r[12:24]) == currents[key] + (0 if key == last_account else totals[key])
        assert number(r[78:90]) == (credits[key] if key == last_account else 0)
        assert number(r[90:102]) == (debits[key] if key == last_account else 0)
    for name in ('TCATBALF', 'XREFFILE', 'DISCGRP', 'TRANFILE'):
        assert (out / f'CBTRN02C.{name}.bin').read_bytes() == (out / f'CBACT04C.{name}.bin').read_bytes()
    assert 'TRY WITH DEFAULT GROUP CODE' in (out / 'CBACT04C.sysout.txt').read_text()
    (out / 'verification.json').write_text(json.dumps({
        'daily_records': len(daily), 'accepted': len(accepted), 'reject_reasons': reasons,
        'interest_records': len(interest), 'verified': True}, indent=2) + '\n')


def manifest(directory):
    return ''.join(
        f'{hashlib.sha256(path.read_bytes()).hexdigest()}  '
        f'modernization/java21/golden/expected/{path.relative_to(directory).as_posix()}\n'
        for path in sorted(directory.rglob('*')) if path.is_file())


def compare():
    baseline = {p.relative_to(EXPECTED) for p in EXPECTED.rglob('*') if p.is_file()}
    generated = {p.relative_to(GENERATED) for p in GENERATED.rglob('*') if p.is_file()}
    differences = []
    for path in sorted(baseline | generated):
        if path not in baseline:
            differences.append(f'Extra generated file: {path}')
        elif path not in generated:
            differences.append(f'Missing generated file: {path}')
        elif (EXPECTED / path).read_bytes() != (GENERATED / path).read_bytes():
            differences.append(f'Differing file: {path}')
    acceptance = HERE / 'acceptance.sha256'
    if not acceptance.is_file() or acceptance.read_text() != manifest(EXPECTED):
        differences.append('Differing or missing file: acceptance.sha256 (baseline manifest)')
    if differences:
        raise SystemExit('\n'.join(differences))
    print(f'CHECK passed: {len(baseline)} files byte-identical; acceptance.sha256 matches')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--update', action='store_true',
                        help='replace expected/ and acceptance.sha256 after verification')
    args = parser.parse_args()
    os.environ.update(TZ='UTC', LC_ALL='C')
    version = subprocess.check_output(['cobc', '-V']).decode().splitlines()[0]
    assert '3.1' in version, version
    if BUILD.exists():
        shutil.rmtree(BUILD)
    GENERATED.mkdir(parents=True)
    for name, (size, key) in SPECS.items():
        bridge(name, size, key)
    flags = ['cobc', '-fsign=EBCDIC', '-I', ROOT / 'app/cpy']
    run(flags + ['-x', '-o', BUILD / 'CBTRN02C', ROOT / 'app/cbl/CBTRN02C.cbl'])
    run(flags + ['-x', '-o', BUILD / 'CBACT04C', HERE / 'interest_driver.cbl',
                 ROOT / 'app/cbl/CBACT04C.cbl'])
    metadata = {'clock_utc': CLOCK, 'parm_date': '2022071800', 'compiler': version,
                'volume_seed': 104, 'volume_records': 3000,
                'sources_sha256': {name: hashlib.sha256((ROOT / 'app/cbl' / name).read_bytes()).hexdigest()
                                   for name in ('CBTRN02C.cbl', 'CBACT04C.cbl')}}
    (GENERATED / 'metadata.json').write_text(json.dumps(metadata, indent=2) + '\n')
    execute('mixed', False)
    execute('volume', True)
    if args.update:
        if EXPECTED.exists():
            shutil.rmtree(EXPECTED)
        shutil.copytree(GENERATED, EXPECTED)
        (HERE / 'acceptance.sha256').write_text(manifest(EXPECTED))
        print('UPDATE complete: expected/ and acceptance.sha256 regenerated after verification')
    else:
        compare()


if __name__ == '__main__':
    main()
