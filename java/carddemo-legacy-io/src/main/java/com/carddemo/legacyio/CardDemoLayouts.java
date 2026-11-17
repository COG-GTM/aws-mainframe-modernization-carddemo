package com.carddemo.legacyio;

import java.util.Map;

/** Record layouts of the CardDemo data files, parsed from the copybooks in {@code app/cpy}. */
public final class CardDemoLayouts {

    /** Account master, ACCTDATA (CVACT01Y, 300 bytes). */
    public static final RecordLayout ACCOUNT = Copybook.load("CVACT01Y").layout();
    /** Card master, CARDDATA (CVACT02Y, 150 bytes). */
    public static final RecordLayout CARD = Copybook.load("CVACT02Y").layout();
    /** Card cross-reference, CARDXREF (CVACT03Y, 50 bytes). */
    public static final RecordLayout CARD_XREF = Copybook.load("CVACT03Y").layout();
    /** Customer master, CUSTDATA (CVCUS01Y, 500 bytes). */
    public static final RecordLayout CUSTOMER = Copybook.load("CVCUS01Y").layout();
    /** Transaction category balance, TCATBALF (CVTRA01Y, 50 bytes). */
    public static final RecordLayout TRAN_CAT_BALANCE = Copybook.load("CVTRA01Y").layout();
    /** Disclosure group, DISCGRP (CVTRA02Y, 50 bytes). */
    public static final RecordLayout DISCLOSURE_GROUP = Copybook.load("CVTRA02Y").layout();
    /** Transaction type, TRANTYPE (CVTRA03Y, 60 bytes). */
    public static final RecordLayout TRAN_TYPE = Copybook.load("CVTRA03Y").layout();
    /** Transaction category, TRANCATG (CVTRA04Y, 60 bytes). */
    public static final RecordLayout TRAN_CATEGORY = Copybook.load("CVTRA04Y").layout();
    /** Posted transaction, TRANSACT (CVTRA05Y, 350 bytes). */
    public static final RecordLayout TRANSACTION = Copybook.load("CVTRA05Y").layout();
    /** Daily transaction input, DALYTRAN (CVTRA06Y, 350 bytes). */
    public static final RecordLayout DAILY_TRANSACTION = Copybook.load("CVTRA06Y").layout();
    /** Sign-on user, USRSEC (CSUSR01Y, 80 bytes). */
    public static final RecordLayout USER_SECURITY = Copybook.load("CSUSR01Y").layout();
    /** Branch export record with the shared data area undivided (CVEXPORT, 500 bytes). */
    public static final RecordLayout EXPORT = Copybook.load("CVEXPORT").layout();

    private static final Map<String, RecordLayout> EXPORT_VIEWS = Map.of(
            "C", EXPORT.view("EXPORT-TIMESTAMP-R", "EXPORT-CUSTOMER-DATA"),
            "A", EXPORT.view("EXPORT-TIMESTAMP-R", "EXPORT-ACCOUNT-DATA"),
            "T", EXPORT.view("EXPORT-TIMESTAMP-R", "EXPORT-TRANSACTION-DATA"),
            "X", EXPORT.view("EXPORT-TIMESTAMP-R", "EXPORT-CARD-XREF-DATA"),
            "D", EXPORT.view("EXPORT-TIMESTAMP-R", "EXPORT-CARD-DATA"));

    /** Picks the CVEXPORT variant from EXPORT-REC-TYPE, as CBEXPORT/CBIMPORT do; unknown types keep the raw area. */
    public static final LayoutSelector EXPORT_BY_TYPE = (record, encoding) ->
            EXPORT_VIEWS.getOrDefault(encoding.decode(record, 0, 1), EXPORT);

    private CardDemoLayouts() {
    }

    /** The CVEXPORT layout for one EXPORT-REC-TYPE value (C, A, T, X or D). */
    public static RecordLayout export(String recordType) {
        RecordLayout layout = EXPORT_VIEWS.get(recordType);
        if (layout == null) {
            throw new IllegalArgumentException("unknown export record type " + recordType);
        }
        return layout;
    }
}
