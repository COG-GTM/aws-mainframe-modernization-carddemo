package com.carddemo.batch.parity;

import java.util.Map;

/** Which golden fields are numeric, and the PIC scale each must decode with. */
final class Scales {

    static final Map<String, Integer> ACCT = Map.of(
            "ACCT-ID", 0, "ACCT-CURR-BAL", 2, "ACCT-CREDIT-LIMIT", 2, "ACCT-CASH-CREDIT-LIMIT", 2,
            "ACCT-CURR-CYC-CREDIT", 2, "ACCT-CURR-CYC-DEBIT", 2);
    static final Map<String, Integer> OUTFILE = Map.of(
            "OUT-ACCT-ID", 0, "OUT-ACCT-CURR-BAL", 2, "OUT-ACCT-CREDIT-LIMIT", 2, "OUT-ACCT-CASH-CREDIT-LIMIT", 2,
            "OUT-ACCT-CURR-CYC-CREDIT", 2, "OUT-ACCT-CURR-CYC-DEBIT", 2);
    static final Map<String, Integer> ARRYFILE = Map.of(
            "ARR-ACCT-ID", 0, "ARR-ACCT-CURR-BAL", 2, "ARR-ACCT-CURR-CYC-DEBIT", 2);
    static final Map<String, Integer> VBRCFILE = Map.of(
            "VB1-ACCT-ID", 0, "VB2-ACCT-ID", 0, "VB2-ACCT-CURR-BAL", 2, "VB2-ACCT-CREDIT-LIMIT", 2);

    static final Map<String, Integer> DALYTRAN = Map.of(
            "DALYTRAN-CAT-CD", 0, "DALYTRAN-AMT", 2, "DALYTRAN-MERCHANT-ID", 0);
    static final Map<String, Integer> CARDXREF = Map.of("XREF-CUST-ID", 0, "XREF-ACCT-ID", 0);
    /** outcomes.json: acct_id is the XREF-ACCT-ID (PIC 9(11)) rendered like a records.py number. */
    static final Map<String, Integer> OUTCOMES = Map.of("acct_id", 0);

    private Scales() {
    }
}
