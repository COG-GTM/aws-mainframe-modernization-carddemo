package com.carddemo.data.record;

import com.carddemo.common.codec.CobolField;

import java.math.BigDecimal;
import java.util.List;

/** IMS segment PAUTSUM0 (COMP-3 amounts, OCCURS 5 statuses). Layout: copybook CIPAUSMY. */
public record PendingAuthSummaryRecord(
        @CobolField("PA-ACCT-ID") Long accountId,
        @CobolField("PA-CUST-ID") Long customerId,
        @CobolField("PA-AUTH-STATUS") String authStatus,
        @CobolField("PA-ACCOUNT-STATUS") List<String> accountStatuses,
        @CobolField("PA-CREDIT-LIMIT") BigDecimal creditLimit,
        @CobolField("PA-CASH-LIMIT") BigDecimal cashLimit,
        @CobolField("PA-CREDIT-BALANCE") BigDecimal creditBalance,
        @CobolField("PA-CASH-BALANCE") BigDecimal cashBalance,
        @CobolField("PA-APPROVED-AUTH-CNT") Integer approvedAuthCount,
        @CobolField("PA-DECLINED-AUTH-CNT") Integer declinedAuthCount,
        @CobolField("PA-APPROVED-AUTH-AMT") BigDecimal approvedAuthAmount,
        @CobolField("PA-DECLINED-AUTH-AMT") BigDecimal declinedAuthAmount) {
}
