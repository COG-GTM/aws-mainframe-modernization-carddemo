/**
 * Dataset wiring for the reporting jobs: the `TRANREPT` and `CREASTMT` JCL
 * steps that prepare the program inputs, over the sample data in
 * `app/data/ASCII`.
 */

import {
  cardXrefCodec,
  cardXrefKey,
  customerCodec,
  customerKey,
  accountCodec,
  accountKey,
  tranCatCodec,
  tranCatKey,
  tranTypeCodec,
  tranTypeKey,
  type TransactionRecord,
} from "@carddemo/domain";
import {
  FileStatus,
  Ksds,
  SequentialWriter,
  dataFiles,
  openDailyTransactionFile,
  type FileStatusCode,
} from "@carddemo/vsam";

import { Cbstm03b } from "./cbstm03b.js";
import { trnxCodec, trnxKey, toTrnxRecord, type TrnxRecord } from "./trnx.js";
import type { DateParm, TransactionReportFiles } from "./transactionReport.js";

/** `DATEPARM` as shipped in the `TRANREPT` JCL. */
export const defaultDateParm: DateParm = { startDate: "2022-01-01", endDate: "2022-07-06" };

/**
 * `AWS.M2.CARDDEMO.TRANSACT`: the posted transactions the report and the
 * statement run over.
 *
 * `CBTRN02C` posts the daily transactions and stamps `TRAN-PROC-TS`; the
 * sample daily file ships with that field blank, so the original timestamp is
 * carried over, which is what the posting step would have recorded.
 */
export function loadPostedTransactions(path = dataFiles.dailytran): TransactionRecord[] {
  const dailyTran = openDailyTransactionFile(path);
  if (dailyTran.openFile() !== FileStatus.ok) {
    throw new Error(`unable to open ${path}`);
  }
  return dailyTran.toArray().map((daily) => ({
    tranId: daily.dalytranId,
    tranTypeCd: daily.dalytranTypeCd,
    tranCatCd: daily.dalytranCatCd,
    tranSource: daily.dalytranSource,
    tranDesc: daily.dalytranDesc,
    tranAmt: daily.dalytranAmt,
    tranMerchantId: daily.dalytranMerchantId,
    tranMerchantName: daily.dalytranMerchantName,
    tranMerchantCity: daily.dalytranMerchantCity,
    tranMerchantZip: daily.dalytranMerchantZip,
    tranCardNum: daily.dalytranCardNum,
    tranOrigTs: daily.dalytranOrigTs,
    tranProcTs: daily.dalytranProcTs.trim().length === 0 ? daily.dalytranOrigTs : daily.dalytranProcTs,
  }));
}

/**
 * `TRANREPT` STEP05R: `INCLUDE COND` on the processing date followed by
 * `SORT FIELDS=(TRAN-CARD-NUM,A)`.
 */
export function sortTransactionsForReport(
  transactions: readonly TransactionRecord[],
  dateParm: DateParm = defaultDateParm,
): TransactionRecord[] {
  return transactions
    .filter((transaction) => {
      const processedDate = transaction.tranProcTs.slice(0, 10);
      return processedDate >= dateParm.startDate && processedDate <= dateParm.endDate;
    })
    .sort((left, right) => left.tranCardNum.localeCompare(right.tranCardNum));
}

/** Opens `CARDXREF`, `TRANTYPE` and `TRANCATG` for `CBTRN03C`. */
export function openTransactionReportFiles(
  transactions: readonly TransactionRecord[],
): TransactionReportFiles {
  const xref = new Ksds({
    name: "CARDXREF",
    path: dataFiles.cardxref,
    codec: cardXrefCodec,
    keyOf: cardXrefKey,
  });
  const tranTypes = new Ksds({
    name: "TRANTYPE",
    path: dataFiles.trantype,
    codec: tranTypeCodec,
    keyOf: tranTypeKey,
  });
  const tranCats = new Ksds({
    name: "TRANCATG",
    path: dataFiles.trancatg,
    codec: tranCatCodec,
    keyOf: tranCatKey,
  });

  for (const store of [xref, tranTypes, tranCats]) {
    const status: FileStatusCode = store.openFile();
    if (status !== FileStatus.ok) {
      throw new Error(`unable to open ${store.name}: file status ${status}`);
    }
  }

  return { transactions, xref, tranTypes, tranCats };
}

/**
 * `CREASTMT` STEP010: sorts the posted transactions on card number and
 * transaction id into the `TRNXFILE` the statement program reads.
 */
export function buildTrnxFile(
  transactions: readonly TransactionRecord[],
  path: string,
): TrnxRecord[] {
  const records = transactions
    .map(toTrnxRecord)
    .sort((left, right) => trnxKey(left).localeCompare(trnxKey(right)));

  const writer = new SequentialWriter({ name: "TRNXFILE", path, codec: trnxCodec });
  writer.openFile();
  for (const record of records) {
    writer.write(record);
  }
  writer.close();
  return records;
}

/** Wires `CBSTM03B` to the four datasets `CBSTM03A` addresses. */
export function openStatementIo(trnxPath: string): Cbstm03b {
  return new Cbstm03b({
    trnxFile: {
      store: new Ksds({
        name: "TRNXFILE",
        path: trnxPath,
        codec: trnxCodec,
        keyOf: trnxKey,
      }),
      codec: trnxCodec,
    },
    xrefFile: {
      store: new Ksds({
        name: "XREFFILE",
        path: dataFiles.cardxref,
        codec: cardXrefCodec,
        keyOf: cardXrefKey,
      }),
      codec: cardXrefCodec,
    },
    custFile: {
      store: new Ksds({
        name: "CUSTFILE",
        path: dataFiles.custdata,
        codec: customerCodec,
        keyOf: customerKey,
      }),
      codec: customerCodec,
    },
    acctFile: {
      store: new Ksds({
        name: "ACCTFILE",
        path: dataFiles.acctdata,
        codec: accountCodec,
        keyOf: accountKey,
      }),
      codec: accountCodec,
    },
  });
}
