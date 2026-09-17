/**
 * Dataset registry: maps the CardDemo VSAM/QSAM DD names to the sample data
 * files in `app/data/ASCII` and builds ready-to-open stores for them.
 */

import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  accountCodec,
  accountKey,
  cardCodec,
  cardKey,
  cardXrefCodec,
  cardXrefKey,
  customerCodec,
  customerKey,
  dailyTransactionCodec,
  discGroupCodec,
  discGroupKey,
  tranCatBalCodec,
  tranCatBalKey,
  tranCatCodec,
  tranCatKey,
  tranTypeCodec,
  tranTypeKey,
  transactionCodec,
  transactionKey,
  type AccountRecord,
  type CardRecord,
  type CardXrefRecord,
  type CustomerRecord,
  type DailyTransactionRecord,
  type DiscGroupRecord,
  type TranCatBalRecord,
  type TranCatRecord,
  type TranTypeRecord,
  type TransactionRecord,
} from "@carddemo/domain";

import { Ksds } from "./ksds.js";
import { SequentialReader } from "./sequential.js";

const packageDir = dirname(dirname(fileURLToPath(import.meta.url)));

/** Root of the repository checkout, used to locate `app/data/ASCII`. */
export const repoRoot = resolve(packageDir, "..", "..", "..");

export const asciiDataDir = join(repoRoot, "app", "data", "ASCII");

export const dataFiles = {
  acctdata: join(asciiDataDir, "acctdata.txt"),
  carddata: join(asciiDataDir, "carddata.txt"),
  cardxref: join(asciiDataDir, "cardxref.txt"),
  custdata: join(asciiDataDir, "custdata.txt"),
  dailytran: join(asciiDataDir, "dailytran.txt"),
  discgrp: join(asciiDataDir, "discgrp.txt"),
  tcatbal: join(asciiDataDir, "tcatbal.txt"),
  trancatg: join(asciiDataDir, "trancatg.txt"),
  trantype: join(asciiDataDir, "trantype.txt"),
} as const;

export type DataFileName = keyof typeof dataFiles;

export const openAccountFile = (path = dataFiles.acctdata): Ksds<AccountRecord> =>
  new Ksds({ name: "ACCTFILE", path, codec: accountCodec, keyOf: accountKey });

export const openCustomerFile = (path = dataFiles.custdata): Ksds<CustomerRecord> =>
  new Ksds({ name: "CUSTFILE", path, codec: customerCodec, keyOf: customerKey });

export const openCardFile = (path = dataFiles.carddata): Ksds<CardRecord> =>
  new Ksds({ name: "CARDFILE", path, codec: cardCodec, keyOf: cardKey });

export const openCardXrefFile = (path = dataFiles.cardxref): Ksds<CardXrefRecord> =>
  new Ksds({ name: "XREFFILE", path, codec: cardXrefCodec, keyOf: cardXrefKey });

export const openDiscGroupFile = (path = dataFiles.discgrp): Ksds<DiscGroupRecord> =>
  new Ksds({ name: "DISCGRP", path, codec: discGroupCodec, keyOf: discGroupKey });

export const openTranCatBalFile = (path = dataFiles.tcatbal): Ksds<TranCatBalRecord> =>
  new Ksds({ name: "TCATBALF", path, codec: tranCatBalCodec, keyOf: tranCatBalKey });

export const openTranTypeFile = (path = dataFiles.trantype): Ksds<TranTypeRecord> =>
  new Ksds({ name: "TRANTYPE", path, codec: tranTypeCodec, keyOf: tranTypeKey });

export const openTranCatFile = (path = dataFiles.trancatg): Ksds<TranCatRecord> =>
  new Ksds({ name: "TRANCATG", path, codec: tranCatCodec, keyOf: tranCatKey });

export const openTransactionFile = (path: string): Ksds<TransactionRecord> =>
  new Ksds({ name: "TRANFILE", path, codec: transactionCodec, keyOf: transactionKey });

export const openDailyTransactionFile = (
  path = dataFiles.dailytran,
): SequentialReader<DailyTransactionRecord> =>
  new SequentialReader({ name: "DALYTRAN", path, codec: dailyTransactionCodec });
