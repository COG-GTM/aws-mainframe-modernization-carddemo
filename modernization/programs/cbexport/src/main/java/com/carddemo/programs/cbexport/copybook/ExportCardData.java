package com.carddemo.programs.cbexport.copybook;

/**
 * CVEXPORT - 05 EXPORT-CARD-DATA REDEFINES EXPORT-RECORD-DATA (460 bytes).
 *
 * @param cardNum        EXP-CARD-NUM PIC X(16)
 * @param acctId         EXP-CARD-ACCT-ID PIC 9(11) COMP (8 bytes)
 * @param cvvCd          EXP-CARD-CVV-CD PIC 9(03) COMP (2 bytes)
 * @param embossedName   EXP-CARD-EMBOSSED-NAME PIC X(50)
 * @param expiraionDate  EXP-CARD-EXPIRAION-DATE PIC X(10) (sic)
 * @param activeStatus   EXP-CARD-ACTIVE-STATUS PIC X(01)
 */
public record ExportCardData(
        String cardNum,
        long acctId,
        int cvvCd,
        String embossedName,
        String expiraionDate,
        String activeStatus) {
}
