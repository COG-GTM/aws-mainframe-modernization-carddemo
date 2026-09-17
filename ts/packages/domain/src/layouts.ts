/**
 * Record layouts transcribed from the COBOL copybooks in `app/cpy`.
 *
 * Each layout keeps the original field order and widths so records round-trip
 * byte-for-byte with the mainframe data files in `app/data/ASCII`.
 */

import { defineLayout, type Layout } from "@carddemo/copybook";

/** `CVACT01Y` — ACCOUNT-RECORD, RECLN 300. */
export const accountLayout: Layout = defineLayout("ACCOUNT-RECORD", [
  { name: "acctId", pic: "9(11)" },
  { name: "acctActiveStatus", pic: "X(01)" },
  { name: "acctCurrBal", pic: "S9(10)V99" },
  { name: "acctCreditLimit", pic: "S9(10)V99" },
  { name: "acctCashCreditLimit", pic: "S9(10)V99" },
  { name: "acctOpenDate", pic: "X(10)" },
  { name: "acctExpiraionDate", pic: "X(10)" },
  { name: "acctReissueDate", pic: "X(10)" },
  { name: "acctCurrCycCredit", pic: "S9(10)V99" },
  { name: "acctCurrCycDebit", pic: "S9(10)V99" },
  { name: "acctAddrZip", pic: "X(10)" },
  { name: "acctGroupId", pic: "X(10)" },
  { pic: "X(178)" },
]);

/** `CVCUS01Y` — CUSTOMER-RECORD, RECLN 500. */
export const customerLayout: Layout = defineLayout("CUSTOMER-RECORD", [
  { name: "custId", pic: "9(09)" },
  { name: "custFirstName", pic: "X(25)" },
  { name: "custMiddleName", pic: "X(25)" },
  { name: "custLastName", pic: "X(25)" },
  { name: "custAddrLine1", pic: "X(50)" },
  { name: "custAddrLine2", pic: "X(50)" },
  { name: "custAddrLine3", pic: "X(50)" },
  { name: "custAddrStateCd", pic: "X(02)" },
  { name: "custAddrCountryCd", pic: "X(03)" },
  { name: "custAddrZip", pic: "X(10)" },
  { name: "custPhoneNum1", pic: "X(15)" },
  { name: "custPhoneNum2", pic: "X(15)" },
  { name: "custSsn", pic: "9(09)" },
  { name: "custGovtIssuedId", pic: "X(20)" },
  { name: "custDobYyyyMmDd", pic: "X(10)" },
  { name: "custEftAccountId", pic: "X(10)" },
  { name: "custPriCardHolderInd", pic: "X(01)" },
  { name: "custFicoCreditScore", pic: "9(03)" },
  { pic: "X(168)" },
]);

/** `CVACT02Y` — CARD-RECORD, RECLN 150. */
export const cardLayout: Layout = defineLayout("CARD-RECORD", [
  { name: "cardNum", pic: "X(16)" },
  { name: "cardAcctId", pic: "9(11)" },
  { name: "cardCvvCd", pic: "9(03)" },
  { name: "cardEmbossedName", pic: "X(50)" },
  { name: "cardExpiraionDate", pic: "X(10)" },
  { name: "cardActiveStatus", pic: "X(01)" },
  { pic: "X(59)" },
]);

/** `CVACT03Y` — CARD-XREF-RECORD, RECLN 50. */
export const cardXrefLayout: Layout = defineLayout("CARD-XREF-RECORD", [
  { name: "xrefCardNum", pic: "X(16)" },
  { name: "xrefCustId", pic: "9(09)" },
  { name: "xrefAcctId", pic: "9(11)" },
  { pic: "X(14)" },
]);

/** `CVTRA05Y` — TRAN-RECORD, RECLN 350. */
export const transactionLayout: Layout = defineLayout("TRAN-RECORD", [
  { name: "tranId", pic: "X(16)" },
  { name: "tranTypeCd", pic: "X(02)" },
  { name: "tranCatCd", pic: "9(04)" },
  { name: "tranSource", pic: "X(10)" },
  { name: "tranDesc", pic: "X(100)" },
  { name: "tranAmt", pic: "S9(09)V99" },
  { name: "tranMerchantId", pic: "9(09)" },
  { name: "tranMerchantName", pic: "X(50)" },
  { name: "tranMerchantCity", pic: "X(50)" },
  { name: "tranMerchantZip", pic: "X(10)" },
  { name: "tranCardNum", pic: "X(16)" },
  { name: "tranOrigTs", pic: "X(26)" },
  { name: "tranProcTs", pic: "X(26)" },
  { pic: "X(20)" },
]);

/** `CVTRA06Y` — DALYTRAN-RECORD, RECLN 350. Same shape as TRAN-RECORD. */
export const dailyTransactionLayout: Layout = defineLayout("DALYTRAN-RECORD", [
  { name: "dalytranId", pic: "X(16)" },
  { name: "dalytranTypeCd", pic: "X(02)" },
  { name: "dalytranCatCd", pic: "9(04)" },
  { name: "dalytranSource", pic: "X(10)" },
  { name: "dalytranDesc", pic: "X(100)" },
  { name: "dalytranAmt", pic: "S9(09)V99" },
  { name: "dalytranMerchantId", pic: "9(09)" },
  { name: "dalytranMerchantName", pic: "X(50)" },
  { name: "dalytranMerchantCity", pic: "X(50)" },
  { name: "dalytranMerchantZip", pic: "X(10)" },
  { name: "dalytranCardNum", pic: "X(16)" },
  { name: "dalytranOrigTs", pic: "X(26)" },
  { name: "dalytranProcTs", pic: "X(26)" },
  { pic: "X(20)" },
]);

/** `CVTRA01Y` — TRAN-CAT-BAL-RECORD, RECLN 50. */
export const tranCatBalLayout: Layout = defineLayout("TRAN-CAT-BAL-RECORD", [
  { name: "trancatAcctId", pic: "9(11)" },
  { name: "trancatTypeCd", pic: "X(02)" },
  { name: "trancatCd", pic: "9(04)" },
  { name: "tranCatBal", pic: "S9(09)V99" },
  { pic: "X(22)" },
]);

/** `CVTRA02Y` — DIS-GROUP-RECORD, RECLN 50. */
export const discGroupLayout: Layout = defineLayout("DIS-GROUP-RECORD", [
  { name: "disAcctGroupId", pic: "X(10)" },
  { name: "disTranTypeCd", pic: "X(02)" },
  { name: "disTranCatCd", pic: "9(04)" },
  { name: "disIntRate", pic: "S9(04)V99" },
  { pic: "X(28)" },
]);

/** `CVTRA03Y` — TRAN-TYPE-RECORD, RECLN 60. */
export const tranTypeLayout: Layout = defineLayout("TRAN-TYPE-RECORD", [
  { name: "tranType", pic: "X(02)" },
  { name: "tranTypeDesc", pic: "X(50)" },
  { pic: "X(08)" },
]);

/** `CVTRA04Y` — TRAN-CAT-RECORD, RECLN 60. */
export const tranCatLayout: Layout = defineLayout("TRAN-CAT-RECORD", [
  { name: "tranTypeCd", pic: "X(02)" },
  { name: "tranCatCd", pic: "9(04)" },
  { name: "tranCatTypeDesc", pic: "X(50)" },
  { pic: "X(04)" },
]);

/** `CSUSR01Y` — SEC-USER-DATA, RECLN 80. */
export const userSecurityLayout: Layout = defineLayout("SEC-USER-DATA", [
  { name: "secUsrId", pic: "X(08)" },
  { name: "secUsrFname", pic: "X(20)" },
  { name: "secUsrLname", pic: "X(20)" },
  { name: "secUsrPwd", pic: "X(08)" },
  { name: "secUsrType", pic: "X(01)" },
  { pic: "X(23)" },
]);
