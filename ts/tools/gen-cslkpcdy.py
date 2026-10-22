"""Regenerates src/common/copybooks/lookup-codes.ts from app/cpy/CSLKPCDY.cpy (run from ts/)."""
import re

src = open('../app/cpy/CSLKPCDY.cpy').read()
lines = [l[6:72] if len(l) > 6 else '' for l in src.splitlines()]
text = '\n'.join(l for l in lines if not l.startswith('*'))
NAMES = ['VALID-PHONE-AREA-CODE', 'VALID-GENERAL-PURP-CODE', 'VALID-EASY-RECOG-AREA-CODE',
         'VALID-US-STATE-CODE', 'VALID-US-STATE-ZIP-CD2-COMBO']


def grab(name):
    m = re.search(r'88 ' + name + r'\s+VALUES(.*?)\.\s*\n', text, re.S)
    return re.findall(r"'([^']*)'", m.group(1))


out = ['/**\n * CSLKPCDY lookup code repository (88-level VALUE lists), generated from\n'
       ' * app/cpy/CSLKPCDY.cpy by tools/gen-cslkpcdy.py. Do not edit by hand.\n */\n']
for n in NAMES:
    vals = grab(n)
    out.append(f'/** 88 {n} ({len(vals)} values). */\nexport const {n.replace("-", "_")}: ReadonlySet<string> = new Set([\n'
               + ''.join(f"  '{v}',\n" for v in vals) + ']);\n')
out.append(open('tools/cslkpcdy-helpers.ts.txt').read())
open('src/common/copybooks/lookup-codes.ts', 'w').write(''.join(out))
