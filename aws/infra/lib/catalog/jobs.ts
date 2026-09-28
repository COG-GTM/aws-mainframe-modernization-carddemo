/**
 * AWS Batch job catalogue from aws/contracts/batch.md §2. One container image (`carddemo-batch`), one job
 * definition per job (`carddemo-<env>-<job>`), command `<prefix> --job=<job> [params]`.
 * `s3Read`/`s3Write` are key prefixes (batch.md §1.2) used to scope each job role; every job may also write
 * `runs/*` (return-code JSON, batch.md §1.1).
 */
export interface JobSpec {
  readonly name: string;
  readonly legacy: string;
  readonly s3Read: string[];
  readonly s3Write: string[];
  /** Only created when the optional authorization (IMS/DB2/MQ) module is enabled. */
  readonly authModule?: boolean;
}

export const JOBS: readonly JobSpec[] = [
  { name: 'post-daily-transactions', legacy: 'POSTTRAN.jcl -> CBTRN02C', s3Read: ['input/dalytran/'], s3Write: ['output/dalyrejs/'] },
  { name: 'calculate-interest', legacy: 'INTCALC.jcl -> CBACT04C', s3Read: [], s3Write: ['output/systran/'] },
  { name: 'combine-transactions', legacy: 'COMBTRAN.jcl', s3Read: ['backup/transaction/', 'output/systran/'], s3Write: [] },
  { name: 'transaction-report', legacy: 'TRANREPT.jcl -> CBTRN03C', s3Read: [], s3Write: ['reports/tranrept/'] },
  { name: 'create-statements', legacy: 'CREASTMT.JCL -> CBSTM03A/CBSTM03B', s3Read: [], s3Write: ['statements/'] },
  { name: 'statement-pdf', legacy: 'TXT2PDF1.JCL', s3Read: ['statements/'], s3Write: ['statements/'] },
  { name: 'backup-transactions', legacy: 'TRANBKP.jcl', s3Read: [], s3Write: ['backup/transaction/'] },
  { name: 'category-balance-report', legacy: 'PRTCATBL.jcl', s3Read: [], s3Write: ['backup/tran_cat_balance/', 'reports/tcatbal/'] },
  { name: 'export-customer-data', legacy: 'CBEXPORT.jcl -> CBEXPORT', s3Read: [], s3Write: ['export/'] },
  { name: 'import-customer-data', legacy: 'CBIMPORT.jcl -> CBIMPORT', s3Read: ['export/'], s3Write: ['import/'] },
  { name: 'extract-accounts', legacy: 'READACCT.jcl -> CBACT01C', s3Read: [], s3Write: ['extract/account/'] },
  { name: 'print-cards', legacy: 'READCARD.jcl -> CBACT02C', s3Read: [], s3Write: [] },
  { name: 'print-xref', legacy: 'READXREF.jcl -> CBACT03C', s3Read: [], s3Write: [] },
  { name: 'print-customers', legacy: 'READCUST.jcl -> CBCUS01C', s3Read: [], s3Write: [] },
  { name: 'validate-daily-transactions', legacy: 'CBTRN01C (no JCL)', s3Read: ['input/dalytran/'], s3Write: [] },
  { name: 'load-reference-data', legacy: 'TRANTYPE/TRANCATG/DISCGRP/TCATBALF/ACCTFILE/CARDFILE/CUSTFILE/XREFFILE/TRANFILE/DUSRSECJ.jcl', s3Read: ['seed/', 'refdata/'], s3Write: [] },
  { name: 'backup-reference-data', legacy: 'DEFGDGD.jcl, TRANEXTR.jcl STEP10/20', s3Read: [], s3Write: ['backup/'] },
  { name: 'maintain-transaction-types', legacy: 'MNTTRDB2.jcl -> COBTUPDT', s3Read: ['input/'], s3Write: [] },
  {
    name: 'extract-transaction-types',
    legacy: 'TRANEXTR.jcl',
    s3Read: [],
    s3Write: ['refdata/transaction_type/', 'refdata/transaction_category/', 'backup/transaction_type/', 'backup/transaction_category/'],
  },
  { name: 'purge-expired-authorizations', legacy: 'CBPAUP0J.jcl -> CBPAUP0C', s3Read: [], s3Write: [], authModule: true },
];

export function jobsFor(enableAuthModule: boolean): JobSpec[] {
  return JOBS.filter((j) => enableAuthModule || !j.authModule);
}

/** Substitution key for a job definition inside an ASL file, e.g. `JobDefinition_post_daily_transactions`. */
export function jobDefinitionKey(job: string): string {
  return `JobDefinition_${job.replace(/-/g, '_')}`;
}
