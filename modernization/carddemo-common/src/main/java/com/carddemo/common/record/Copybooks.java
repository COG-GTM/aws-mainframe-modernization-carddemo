package com.carddemo.common.record;

/** Layouts of the copybooks POSTTRAN uses, field for field from {@code app/cpy} and {@code CBTRN02C.cbl}. */
public final class Copybooks {

    /** {@code app/cpy/CVTRA06Y.cpy} DALYTRAN-RECORD, RECL 350. */
    public static final RecordLayout CVTRA06Y = transaction("CVTRA06Y", "DALYTRAN");

    /** {@code app/cpy/CVTRA05Y.cpy} TRAN-RECORD, RECL 350. */
    public static final RecordLayout CVTRA05Y = transaction("CVTRA05Y", "TRAN");

    /** {@code app/cpy/CVACT01Y.cpy} ACCOUNT-RECORD, RECL 300. */
    public static final RecordLayout CVACT01Y = RecordLayout.builder("CVACT01Y", 300)
            .unsigned("ACCT-ID", 11)
            .alpha("ACCT-ACTIVE-STATUS", 1)
            .signed("ACCT-CURR-BAL", 10, 2)
            .signed("ACCT-CREDIT-LIMIT", 10, 2)
            .signed("ACCT-CASH-CREDIT-LIMIT", 10, 2)
            .alpha("ACCT-OPEN-DATE", 10)
            .alpha("ACCT-EXPIRAION-DATE", 10)
            .alpha("ACCT-REISSUE-DATE", 10)
            .signed("ACCT-CURR-CYC-CREDIT", 10, 2)
            .signed("ACCT-CURR-CYC-DEBIT", 10, 2)
            .alpha("ACCT-ADDR-ZIP", 10)
            .alpha("ACCT-GROUP-ID", 10)
            .filler(178)
            .build();

    /** {@code app/cpy/CVACT03Y.cpy} CARD-XREF-RECORD, RECL 50. */
    public static final RecordLayout CVACT03Y = RecordLayout.builder("CVACT03Y", 50)
            .alpha("XREF-CARD-NUM", 16)
            .unsigned("XREF-CUST-ID", 9)
            .unsigned("XREF-ACCT-ID", 11)
            .filler(14)
            .build();

    /**
     * {@code app/cpy/CVTRA01Y.cpy} TRAN-CAT-BAL-RECORD, RECL 50. FILLER is emitted as zeros: every sample record and every
     * oracle-written record carries 22 zeros there (D-036).
     */
    public static final RecordLayout CVTRA01Y = RecordLayout.builder("CVTRA01Y", 50)
            .unsigned("TRANCAT-ACCT-ID", 11)
            .alpha("TRANCAT-TYPE-CD", 2)
            .unsigned("TRANCAT-CD", 4)
            .signed("TRAN-CAT-BAL", 9, 2)
            .filler(22, '0')
            .build();

    /**
     * DALYREJS record, RECL 430 ({@code CBTRN02C.cbl} REJECT-RECORD + WS-VALIDATION-TRAILER): the DALYTRAN record
     * verbatim, {@code WS-VALIDATION-FAIL-REASON PIC 9(04)}, {@code WS-VALIDATION-FAIL-REASON-DESC PIC X(76)}.
     */
    public static final RecordLayout DALYREJS = RecordLayout.builder("DALYREJS", 430)
            .alpha("REJECT-TRAN-DATA", 350)
            .unsigned("WS-VALIDATION-FAIL-REASON", 4)
            .alpha("WS-VALIDATION-FAIL-REASON-DESC", 76)
            .build();

    private Copybooks() {
    }

    private static RecordLayout transaction(String copybook, String prefix) {
        return RecordLayout.builder(copybook, 350)
                .alpha(prefix + "-ID", 16)
                .alpha(prefix + "-TYPE-CD", 2)
                .unsigned(prefix + "-CAT-CD", 4)
                .alpha(prefix + "-SOURCE", 10)
                .alpha(prefix + "-DESC", 100)
                .signed(prefix + "-AMT", 9, 2)
                .unsigned(prefix + "-MERCHANT-ID", 9)
                .alpha(prefix + "-MERCHANT-NAME", 50)
                .alpha(prefix + "-MERCHANT-CITY", 50)
                .alpha(prefix + "-MERCHANT-ZIP", 10)
                .alpha(prefix + "-CARD-NUM", 16)
                .alpha(prefix + "-ORIG-TS", 26)
                .alpha(prefix + "-PROC-TS", 26)
                .filler(20)
                .build();
    }
}
