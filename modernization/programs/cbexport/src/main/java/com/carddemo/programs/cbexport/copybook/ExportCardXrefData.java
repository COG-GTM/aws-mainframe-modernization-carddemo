package com.carddemo.programs.cbexport.copybook;

/**
 * CVEXPORT - 05 EXPORT-CARD-XREF-DATA REDEFINES EXPORT-RECORD-DATA (460 bytes).
 *
 * @param cardNum EXP-XREF-CARD-NUM PIC X(16)
 * @param custId  EXP-XREF-CUST-ID PIC 9(09)
 * @param acctId  EXP-XREF-ACCT-ID PIC 9(11) COMP (8 bytes)
 */
public record ExportCardXrefData(String cardNum, long custId, long acctId) {
}
