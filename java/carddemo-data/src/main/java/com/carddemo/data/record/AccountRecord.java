package com.carddemo.data.record;

import com.carddemo.common.codec.CobolField;

import java.math.BigDecimal;

/** Fixed-width ACCOUNT-RECORD (dates kept as their PIC X text). Layout: copybook CVACT01Y. */
public record AccountRecord(
        @CobolField("ACCT-ID") Long id,
        @CobolField("ACCT-ACTIVE-STATUS") String activeStatus,
        @CobolField("ACCT-CURR-BAL") BigDecimal currentBalance,
        @CobolField("ACCT-CREDIT-LIMIT") BigDecimal creditLimit,
        @CobolField("ACCT-CASH-CREDIT-LIMIT") BigDecimal cashCreditLimit,
        @CobolField("ACCT-OPEN-DATE") String openDate,
        @CobolField("ACCT-EXPIRAION-DATE") String expirationDate,
        @CobolField("ACCT-REISSUE-DATE") String reissueDate,
        @CobolField("ACCT-CURR-CYC-CREDIT") BigDecimal currentCycleCredit,
        @CobolField("ACCT-CURR-CYC-DEBIT") BigDecimal currentCycleDebit,
        @CobolField("ACCT-ADDR-ZIP") String addressZip,
        @CobolField("ACCT-GROUP-ID") String groupId) {
}
