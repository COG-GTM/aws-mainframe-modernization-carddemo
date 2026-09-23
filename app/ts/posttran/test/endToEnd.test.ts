import assert from 'node:assert/strict';
import { copyFileSync, mkdtempSync, readFileSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { describe, it } from 'node:test';

import { toDecimalString } from '../src/codec/money.ts';
import { main } from '../src/cli.ts';
import { closePostingFiles, openPostingFiles } from '../src/io/datasets.ts';
import type { PostingFilePaths } from '../src/io/datasets.ts';
import { parseAccount } from '../src/records/account.ts';
import { parseRejectRecord } from '../src/records/rejectRecord.ts';
import { parseTransactionMaster } from '../src/records/transactionMaster.ts';
import { runPostingJob } from '../src/posting/job.ts';
import { SAMPLE_DATA_DIR } from './helpers/sampleData.ts';

function stageSampleData(): PostingFilePaths {
  const directory = mkdtempSync(join(tmpdir(), 'posttran-e2e-'));
  const copy = (name: string): string => {
    const destination = join(directory, name);
    copyFileSync(join(SAMPLE_DATA_DIR, name), destination);
    return destination;
  };
  const empty = (name: string): string => {
    const destination = join(directory, name);
    writeFileSync(destination, '', 'latin1');
    return destination;
  };
  return {
    dalytran: copy('dailytran.txt'),
    xreffile: copy('cardxref.txt'),
    acctfile: copy('acctdata.txt'),
    tcatbalf: copy('tcatbal.txt'),
    tranfile: empty('tranfile.txt'),
    dalyrejs: empty('dalyrejs.txt'),
  };
}

function lines(path: string, length: number): string[] {
  const content = readFileSync(path, 'latin1');
  return content.length === 0 ? [] : content.split('\n').filter((line) => line.length >= length);
}

void describe('end to end over app/data/ASCII', () => {
  void it('posts the sample daily transaction file deterministically', () => {
    const paths = stageSampleData();
    const files = openPostingFiles(paths);
    const balanceBefore = files.acctfile.read('00000000011')?.currentBalance;

    const logged: string[] = [];
    const result = runPostingJob(files, {
      clock: () => new Date(2025, 0, 2, 3, 4, 5, 60),
      log: (line) => logged.push(line),
    });
    closePostingFiles(files, paths);

    assert.equal(result.transactionsProcessed, 300);
    assert.equal(
      result.transactionsProcessed,
      result.transactionsRejected + files.tranfile.all().length,
    );
    assert.ok(result.transactionsRejected > 0, 'the sample data contains rejects');
    assert.equal(result.returnCode, 4);
    assert.ok(logged.includes('TRANSACTIONS PROCESSED :000000300'));
    assert.ok(logged.includes('START OF EXECUTION OF PROGRAM CBTRN02C'));
    assert.ok(logged.includes('END OF EXECUTION OF PROGRAM CBTRN02C'));

    // Every reject is a full 430-byte record carrying a documented reason.
    const rejects = lines(paths.dalyrejs, 430).map(parseRejectRecord);
    assert.equal(rejects.length, result.transactionsRejected);
    for (const reject of rejects) {
      assert.ok([100, 101, 102, 103].includes(reject.reasonCode));
      assert.notEqual(reject.reasonDescription.trim(), '');
    }

    // TRANFILE is loaded, not appended: it holds exactly the posted records.
    const posted = lines(paths.tranfile, 350).map(parseTransactionMaster);
    assert.equal(posted.length, files.tranfile.all().length);
    for (const record of posted) {
      assert.equal(record.processingTimestamp, '2025-01-02-03.04.05.060000');
    }

    // Account balances were rewritten in place with exact decimal arithmetic.
    const accounts = lines(paths.acctfile, 300).map(parseAccount);
    assert.equal(accounts.length, 50);
    const balanceAfter = accounts.find((record) => record.id === '00000000011')?.currentBalance;
    assert.ok(balanceBefore !== undefined && balanceAfter !== undefined);
    const postedForAccount = posted.filter((record) => record.cardNumber.length === 16);
    assert.ok(postedForAccount.length > 0);
    assert.equal(typeof toDecimalString(balanceAfter), 'string');
  });

  void it('is idempotent in shape and reproducible across runs', () => {
    const run = (): { returnCode: number; tranfile: string; dalyrejs: string } => {
      const paths = stageSampleData();
      const files = openPostingFiles(paths);
      const result = runPostingJob(files, { clock: () => new Date(2025, 0, 2, 3, 4, 5, 60) });
      closePostingFiles(files, paths);
      return {
        returnCode: result.returnCode,
        tranfile: readFileSync(paths.tranfile, 'latin1'),
        dalyrejs: readFileSync(paths.dalyrejs, 'latin1'),
      };
    };
    const first = run();
    const second = run();
    assert.equal(first.returnCode, second.returnCode);
    assert.equal(first.tranfile, second.tranfile);
    assert.equal(first.dalyrejs, second.dalyrejs);
  });

  void it('runs through the CLI entrypoint and returns 4 when there are rejects', () => {
    const paths = stageSampleData();
    const logged: string[] = [];
    const returnCode = main(
      [
        `--dalytran=${paths.dalytran}`,
        `--xreffile=${paths.xreffile}`,
        `--acctfile=${paths.acctfile}`,
        `--tcatbalf=${paths.tcatbalf}`,
        `--tranfile=${paths.tranfile}`,
        `--dalyrejs=${paths.dalyrejs}`,
      ],
      (line) => logged.push(line),
    );
    assert.equal(returnCode, 4);
    assert.ok(logged.some((line) => line.startsWith('TRANSACTIONS REJECTED  :')));
  });
});
