package com.carddemo.programs.cbexport.copybook;

import java.math.BigDecimal;

/**
 * CVEXPORT - 05 EXPORT-ACCOUNT-DATA REDEFINES EXPORT-RECORD-DATA (460 bytes). Note the mixed
 * USAGEs, which are preserved byte-for-byte.
 *
 * @param acctId            EXP-ACCT-ID PIC 9(11)
 * @param activeStatus      EXP-ACCT-ACTIVE-STATUS PIC X(01)
 * @param currBal           EXP-ACCT-CURR-BAL PIC S9(10)V99 COMP-3 (7 bytes)
 * @param creditLimit       EXP-ACCT-CREDIT-LIMIT PIC S9(10)V99 (DISPLAY, 12 bytes)
 * @param cashCreditLimit   EXP-ACCT-CASH-CREDIT-LIMIT PIC S9(10)V99 COMP-3 (7 bytes)
 * @param openDate          EXP-ACCT-OPEN-DATE PIC X(10)
 * @param expiraionDate     EXP-ACCT-EXPIRAION-DATE PIC X(10) (sic)
 * @param reissueDate       EXP-ACCT-REISSUE-DATE PIC X(10)
 * @param currCycCredit     EXP-ACCT-CURR-CYC-CREDIT PIC S9(10)V99 (DISPLAY, 12 bytes)
 * @param currCycDebit      EXP-ACCT-CURR-CYC-DEBIT PIC S9(10)V99 COMP (8 bytes)
 * @param addrZip           EXP-ACCT-ADDR-ZIP PIC X(10)
 * @param groupId           EXP-ACCT-GROUP-ID PIC X(10)
 */
public record ExportAccountData(
        long acctId,
        String activeStatus,
        BigDecimal currBal,
        BigDecimal creditLimit,
        BigDecimal cashCreditLimit,
        String openDate,
        String expiraionDate,
        String reissueDate,
        BigDecimal currCycCredit,
        BigDecimal currCycDebit,
        String addrZip,
        String groupId) {
}
