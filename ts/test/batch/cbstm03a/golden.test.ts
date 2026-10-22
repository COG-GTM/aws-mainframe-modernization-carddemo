/**
 * Parity with GnuCOBOL: golden/statement.{txt,html} were produced by running
 * the real CBSTM03A + CBSTM03B under GnuCOBOL 3.1 over the seed data, with
 * TRNXFILE built from dailytran.txt as CREASTMT does (tools/gen-golden-cbstm03a.sh).
 */
import { mkdtempSync, readFileSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { describe, expect, it } from 'vitest';
import { buildTrnxFile, runCbstm03a } from '../../../src/batch/cbstm03a/index.js';
import { loadSeedCatalog } from '../../../src/common/seed/index.js';

const golden = (f: string) => readFileSync(join(import.meta.dirname, 'golden', f), 'latin1').split('\n').slice(0, -1);
const rtrim = (lines: string[]) => lines.map((l) => l.trimEnd());

function run() {
  const { catalog } = loadSeedCatalog({ transactFromDailytran: true });
  const built = buildTrnxFile(catalog);
  return { built, result: runCbstm03a({ catalog }) };
}

describe('CBSTM03A golden (GnuCOBOL)', () => {
  const { built, result } = run();

  it('builds TRNXFILE from all 300 daily transactions', () => {
    expect(built).toMatchObject({ loaded: 300, rejected: 0 });
  });

  it('plain-text statements match line for line', () => {
    const expected = golden('statement.txt');
    expect(expected).toHaveLength(1250);
    expect(result.stmtRecords.every((r) => r.length === 80)).toBe(true);
    expect(rtrim(result.stmtRecords)).toEqual(expected);
  });

  it('HTML statements match line for line', () => {
    const expected = golden('statement.html');
    expect(expected).toHaveLength(6500);
    expect(result.htmlRecords.every((r) => r.length === 100)).toBe(true);
    expect(rtrim(result.htmlRecords)).toEqual(expected);
  });

  it('writes STMTFILE / HTMLFILE as line-sequential files', () => {
    const dir = mkdtempSync(join(tmpdir(), 'cbstm03a-'));
    try {
      const { catalog } = loadSeedCatalog({ transactFromDailytran: true });
      buildTrnxFile(catalog);
      runCbstm03a({ catalog, job: null, stmtFile: join(dir, 'stmt.txt'), htmlFile: join(dir, 'stmt.html'), trimTrailing: true });
      expect(readFileSync(join(dir, 'stmt.txt'), 'latin1').split('\n').slice(0, -1)).toEqual(golden('statement.txt'));
      expect(readFileSync(join(dir, 'stmt.html'), 'latin1').split('\n').slice(0, -1)).toEqual(golden('statement.html'));
    } finally {
      rmSync(dir, { recursive: true, force: true });
    }
  });
});
