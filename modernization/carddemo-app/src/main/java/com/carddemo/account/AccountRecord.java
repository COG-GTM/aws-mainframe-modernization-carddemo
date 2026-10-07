package com.carddemo.account;

import com.carddemo.common.data.CobolField;
import com.carddemo.common.data.CopybookRecordMapper;
import java.math.BigDecimal;

/**
 * One ACCTDATA record (copybook CVACT01Y) as an immutable value. {@link #MAPPER} converts it to and from
 * the fixed-width record; {@link Account} persists it.
 *
 * @param acctId ACCT-ID PIC 9(11)
 * @param activeStatus ACCT-ACTIVE-STATUS PIC X(01)
 * @param currBal ACCT-CURR-BAL PIC S9(10)V99
 * @param creditLimit ACCT-CREDIT-LIMIT PIC S9(10)V99
 * @param cashCreditLimit ACCT-CASH-CREDIT-LIMIT PIC S9(10)V99
 * @param openDate ACCT-OPEN-DATE PIC X(10)
 * @param expirationDate ACCT-EXPIRAION-DATE PIC X(10)
 * @param reissueDate ACCT-REISSUE-DATE PIC X(10)
 * @param currCycCredit ACCT-CURR-CYC-CREDIT PIC S9(10)V99
 * @param currCycDebit ACCT-CURR-CYC-DEBIT PIC S9(10)V99
 * @param addrZip ACCT-ADDR-ZIP PIC X(10)
 * @param groupId ACCT-GROUP-ID PIC X(10)
 */
public record AccountRecord(
        @CobolField("ACCT-ID") long acctId,
        @CobolField("ACCT-ACTIVE-STATUS") AccountStatus activeStatus,
        @CobolField("ACCT-CURR-BAL") BigDecimal currBal,
        @CobolField("ACCT-CREDIT-LIMIT") BigDecimal creditLimit,
        @CobolField("ACCT-CASH-CREDIT-LIMIT") BigDecimal cashCreditLimit,
        @CobolField("ACCT-OPEN-DATE") String openDate,
        @CobolField("ACCT-EXPIRAION-DATE") String expirationDate,
        @CobolField("ACCT-REISSUE-DATE") String reissueDate,
        @CobolField("ACCT-CURR-CYC-CREDIT") BigDecimal currCycCredit,
        @CobolField("ACCT-CURR-CYC-DEBIT") BigDecimal currCycDebit,
        @CobolField("ACCT-ADDR-ZIP") String addrZip,
        @CobolField("ACCT-GROUP-ID") String groupId) {

    public static final CopybookRecordMapper<AccountRecord> MAPPER =
            CopybookRecordMapper.of(AccountRecord.class, "CVACT01Y");
}
