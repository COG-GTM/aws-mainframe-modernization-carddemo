/**
 * CBACT01C: reads the account KSDS sequentially, DISPLAYs every record and
 * writes three output files per account: OUTFILE (107-byte FB, one COMP-3
 * field), ARRYFILE (110-byte FB, OCCURS 5 with COMP-3) and VBRCFILE (VB,
 * a 12-byte and a 39-byte record). Port of app/cbl/CBACT01C.cbl.
 */
import { AccountRecordLayout } from '../../common/copybooks/account.js';
import { alnum, defineRecord, group, num, occurs } from '../../common/record/layout.js';
import { IndexedFile } from '../../common/vsam/cobol-file.js';
import { abendProgram, Decimal, OutputFile, displayIoStatus, resolveCatalog, runProgram, type BatchRunOptions, type BatchRunResult, type Sysout } from '../acctcust-common/support.js';
import { fit } from '../acctcust-common/ksds-print.js';
import { cobdatft, CODATECN_LENGTH } from './cobdatft.js';

export { cobdatft } from './cobdatft.js';

/** FD OUT-FILE / OUT-ACCT-REC (LRECL 107). */
export const OutAcctRecLayout = defineRecord('OUT-ACCT-REC', {
  outAcctId: num('OUT-ACCT-ID', '9(11)'),
  outAcctActiveStatus: alnum('OUT-ACCT-ACTIVE-STATUS', 'X(01)'),
  outAcctCurrBal: num('OUT-ACCT-CURR-BAL', 'S9(10)V99'),
  outAcctCreditLimit: num('OUT-ACCT-CREDIT-LIMIT', 'S9(10)V99'),
  outAcctCashCreditLimit: num('OUT-ACCT-CASH-CREDIT-LIMIT', 'S9(10)V99'),
  outAcctOpenDate: alnum('OUT-ACCT-OPEN-DATE', 'X(10)'),
  outAcctExpiraionDate: alnum('OUT-ACCT-EXPIRAION-DATE', 'X(10)'),
  outAcctReissueDate: alnum('OUT-ACCT-REISSUE-DATE', 'X(10)'),
  outAcctCurrCycCredit: num('OUT-ACCT-CURR-CYC-CREDIT', 'S9(10)V99'),
  outAcctCurrCycDebit: num('OUT-ACCT-CURR-CYC-DEBIT', 'S9(10)V99', { usage: 'COMP-3' }),
  outAcctGroupId: alnum('OUT-ACCT-GROUP-ID', 'X(10)'),
});

/** FD ARRY-FILE / ARR-ARRAY-REC (LRECL 110). */
export const ArrArrayRecLayout = defineRecord('ARR-ARRAY-REC', {
  arrAcctId: num('ARR-ACCT-ID', '9(11)'),
  arrAcctBal: occurs(
    5,
    group('ARR-ACCT-BAL', {
      arrAcctCurrBal: num('ARR-ACCT-CURR-BAL', 'S9(10)V99'),
      arrAcctCurrCycDebit: num('ARR-ACCT-CURR-CYC-DEBIT', 'S9(10)V99', { usage: 'COMP-3' }),
    }),
  ),
  arrFiller: alnum('ARR-FILLER', 'X(04)'),
});

/** WORKING-STORAGE VBRC-REC1 (12 bytes). */
export const VbrcRec1Layout = defineRecord('VBRC-REC1', {
  vb1AcctId: num('VB1-ACCT-ID', '9(11)'),
  vb1AcctActiveStatus: alnum('VB1-ACCT-ACTIVE-STATUS', 'X(01)'),
});

/** WORKING-STORAGE VBRC-REC2 (39 bytes). */
export const VbrcRec2Layout = defineRecord('VBRC-REC2', {
  vb2AcctId: num('VB2-ACCT-ID', '9(11)'),
  vb2AcctCurrBal: num('VB2-ACCT-CURR-BAL', 'S9(10)V99'),
  vb2AcctCreditLimit: num('VB2-ACCT-CREDIT-LIMIT', 'S9(10)V99'),
  vb2AcctReissueYyyy: alnum('VB2-ACCT-REISSUE-YYYY', 'X(04)'),
});

export interface Cbact01cOptions extends BatchRunOptions {
  /** OUTFILE path (RECFM=FB LRECL=107, no delimiters). Not written when omitted. */
  outFile?: string;
  /** ARRYFILE path (RECFM=FB LRECL=110). */
  arryFile?: string;
  /** VBRCFILE path (RECFM=VB: each record prefixed by a 4-byte RDW, length incl. RDW, big-endian). */
  vbrcFile?: string;
}

export interface Cbact01cResult extends BatchRunResult {
  outRecords: Buffer[];
  arryRecords: Buffer[];
  /** VB record data (without RDW). */
  vbrcRecords: Buffer[];
}

const ACCT_DISPLAY_FIELDS: readonly [string, string][] = [
  ['ACCT-ID                 :', 'ACCT-ID'],
  ['ACCT-ACTIVE-STATUS      :', 'ACCT-ACTIVE-STATUS'],
  ['ACCT-CURR-BAL           :', 'ACCT-CURR-BAL'],
  ['ACCT-CREDIT-LIMIT       :', 'ACCT-CREDIT-LIMIT'],
  ['ACCT-CASH-CREDIT-LIMIT  :', 'ACCT-CASH-CREDIT-LIMIT'],
  ['ACCT-OPEN-DATE          :', 'ACCT-OPEN-DATE'],
  ['ACCT-EXPIRAION-DATE     :', 'ACCT-EXPIRAION-DATE'],
  ['ACCT-REISSUE-DATE       :', 'ACCT-REISSUE-DATE'],
  ['ACCT-CURR-CYC-CREDIT    :', 'ACCT-CURR-CYC-CREDIT'],
  ['ACCT-CURR-CYC-DEBIT     :', 'ACCT-CURR-CYC-DEBIT'],
  ['ACCT-GROUP-ID           :', 'ACCT-GROUP-ID'],
];

export function runCbact01c(opts: Cbact01cOptions = {}): Cbact01cResult {
  const outRecords: Buffer[] = [];
  const arryRecords: Buffer[] = [];
  const vbrcRecords: Buffer[] = [];
  const outFiles = [new OutputFile(opts.outFile, false), new OutputFile(opts.arryFile, false), new OutputFile(opts.vbrcFile, true)] as const;
  const [outFile, arryFile, vbrcFile] = outFiles;
  try {
    return runProgram(
      opts,
      (out) => {
        const acctFile = new IndexedFile(resolveCatalog(opts), 'ACCTFILE', { access: 'SEQUENTIAL' });
        let account = AccountRecordLayout.serialize({});
        // CODATECN-REC (WORKING-STORAGE, no VALUE): spaces, as GnuCOBOL initializes it.
        const codatecn = Buffer.alloc(CODATECN_LENGTH, 0x20);
        // FD record area of OUT-FILE: only partly MOVEd per record, so earlier content survives.
        const outArea = OutAcctRecLayout.serialize({});
        out.display('START OF EXECUTION OF PROGRAM CBACT01C');
        openCheck(out, acctFile.open('INPUT').status, 'ERROR OPENING ACCTFILE', '');
        const s2 = outFile.open();
        openCheck(out, s2, 'ERROR OPENING OUTFILE', s2);
        const s3 = arryFile.open();
        openCheck(out, s3, 'ERROR OPENING ARRAYFILE', s3);
        const s4 = vbrcFile.open();
        openCheck(out, s4, 'ERROR OPENING VBRC FILE', s4);

        let endOfFile = false;
        while (!endOfFile) {
          const res = acctFile.readNext();
          if (res.status === '00') {
            account = fit(res.record!, AccountRecordLayout.length);
            displayAcctRecord(out, account);
            const acct = AccountRecordLayout.parse(account);

            // 1300-POPUL-ACCT-RECORD
            OutAcctRecLayout.serialize({
              outAcctId: acct.acctId,
              outAcctActiveStatus: acct.acctActiveStatus,
              outAcctCurrBal: acct.acctCurrBal,
              outAcctCreditLimit: acct.acctCreditLimit,
              outAcctCashCreditLimit: acct.acctCashCreditLimit,
              outAcctOpenDate: acct.acctOpenDate,
              outAcctExpiraionDate: acct.acctExpiraionDate,
            }).copy(outArea, 0, 0, OutAcctRecLayout.range('OUT-ACCT-REISSUE-DATE').offset);
            codatecn.write(acct.acctReissueDate.padEnd(20, ' '), 1, 20, 'latin1');
            const wsReissueDate = acct.acctReissueDate;
            codatecn.write('2', 0, 'latin1');
            codatecn.write('2', 21, 'latin1');
            cobdatft(codatecn);
            putField(outArea, 'OUT-ACCT-REISSUE-DATE', codatecn.subarray(22, 32));
            putField(outArea, 'OUT-ACCT-CURR-CYC-CREDIT', numBytes('OUT-ACCT-CURR-CYC-CREDIT', acct.acctCurrCycCredit));
            if (acct.acctCurrCycDebit.isZero()) {
              putField(outArea, 'OUT-ACCT-CURR-CYC-DEBIT', numBytes('OUT-ACCT-CURR-CYC-DEBIT', new Decimal('2525.00')));
            }
            putField(outArea, 'OUT-ACCT-GROUP-ID', Buffer.from(acct.acctGroupId, 'latin1'));
            writeCheck(out, outFile.write(outArea));
            outRecords.push(Buffer.from(outArea));

            // INITIALIZE ARR-ARRAY-REC + 1400-POPUL-ARRAY-RECORD
            const arr = ArrArrayRecLayout.serialize({
              arrAcctId: acct.acctId,
              arrAcctBal: [
                { arrAcctCurrBal: acct.acctCurrBal, arrAcctCurrCycDebit: '1005.00' },
                { arrAcctCurrBal: acct.acctCurrBal, arrAcctCurrCycDebit: '1525.00' },
                { arrAcctCurrBal: '-1025.00', arrAcctCurrCycDebit: '-2500.00' },
              ],
            });
            writeCheck(out, arryFile.write(arr));
            arryRecords.push(arr);

            // INITIALIZE VBRC-REC1 + 1500-POPUL-VBRC-RECORD
            const vb1 = VbrcRec1Layout.serialize({ vb1AcctId: acct.acctId, vb1AcctActiveStatus: acct.acctActiveStatus });
            const vb2 = VbrcRec2Layout.serialize({
              vb2AcctId: acct.acctId,
              vb2AcctCurrBal: acct.acctCurrBal,
              vb2AcctCreditLimit: acct.acctCreditLimit,
              vb2AcctReissueYyyy: wsReissueDate.slice(0, 4),
            });
            out.display('VBRC-REC1:', vb1);
            out.display('VBRC-REC2:', vb2);
            writeCheck(out, vbrcFile.write(vb1));
            vbrcRecords.push(vb1);
            writeCheck(out, vbrcFile.write(vb2));
            vbrcRecords.push(vb2);
          } else if (res.status === '10') {
            endOfFile = true;
          } else {
            out.display('ERROR READING ACCOUNT FILE');
            displayIoStatus(out, res.status);
            abendProgram(out);
          }
          if (!endOfFile) out.display(account);
        }

        const cs = acctFile.close().status;
        if (cs !== '00') {
          out.display('ERROR CLOSING ACCOUNT FILE');
          displayIoStatus(out, cs);
          abendProgram(out);
        }
        out.display('END OF EXECUTION OF PROGRAM CBACT01C');
        return { outRecords, arryRecords, vbrcRecords };
      },
      () => ({ outRecords, arryRecords, vbrcRecords }),
    );
  } finally {
    for (const f of outFiles) f.close();
  }
}

/** 1100-DISPLAY-ACCT-RECORD: DISPLAY of a DISPLAY-usage item shows its bytes (zoned signs as overpunch). */
function displayAcctRecord(out: Sysout, account: Buffer): void {
  for (const [label, name] of ACCT_DISPLAY_FIELDS) {
    const { offset, length } = AccountRecordLayout.range(name);
    out.display(label, account.subarray(offset, offset + length));
  }
  out.display('-------------------------------------------------');
}

function putField(area: Buffer, name: string, bytes: Buffer): void {
  const { offset, length } = OutAcctRecLayout.range(name);
  const b = Buffer.alloc(length, 0x20);
  bytes.copy(b, 0, 0, Math.min(length, bytes.length));
  b.copy(area, offset);
}

function numBytes(name: string, value: Decimal): Buffer {
  const { offset, length } = OutAcctRecLayout.range(name);
  const key = name === 'OUT-ACCT-CURR-CYC-DEBIT' ? 'outAcctCurrCycDebit' : 'outAcctCurrCycCredit';
  return OutAcctRecLayout.serialize({ [key]: value }).subarray(offset, offset + length);
}

function openCheck(out: Sysout, status: string, message: string, suffix: string): void {
  if (status === '00') return;
  out.display(message, suffix);
  displayIoStatus(out, status);
  abendProgram(out);
}

function writeCheck(out: Sysout, status: string): void {
  if (status === '00' || status === '10') return;
  out.display('ACCOUNT FILE WRITE STATUS IS:', status);
  displayIoStatus(out, status);
  abendProgram(out);
}
