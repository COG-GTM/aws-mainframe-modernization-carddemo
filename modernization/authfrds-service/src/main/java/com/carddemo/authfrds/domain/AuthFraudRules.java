package com.carddemo.authfrds.domain;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

/** Pure rules from COPAUS2C / COPAUS1C. Message text is copied from the COBOL literals. */
public final class AuthFraudRules {

    public static final String ADD_SUCCESS = "ADD SUCCESS";
    public static final String UPDT_SUCCESS = "UPDT SUCCESS";
    public static final String AUTH_MARKED_FRAUD = "AUTH MARKED FRAUD...";
    public static final String AUTH_FRAUD_REMOVED = "AUTH FRAUD REMOVED...";
    public static final String FRAUD_TAGGING_ROLLBACK = " System error while FRAUD Tagging, ROLLBACK||";
    public static final String READ_SUMMARY_ERROR = " System error while reading Auth Summary: Code:";
    public static final String READ_DETAILS_ERROR = " System error while reading Auth Details: Code:";
    public static final String READ_NEXT_ERROR = " System error while reading next Auth: Code:";
    public static final String LAST_AUTHORIZATION = "Already at the last Authorization...";
    public static final String INVALID_KEY = "Invalid key pressed. Please see below...";
    public static final String DB2_TIMESTAMP_FORMAT = "YY-MM-DD HH24.MI.SSNNNNNN";

    static final int ACT_MSG_LENGTH = 50;
    static final int AUTH_TS_HOST_LENGTH = 26;
    static final int MERCHANT_NAME_LENGTH = 22;
    private static final DateTimeFormatter MMDDYY = DateTimeFormatter.ofPattern("MM/dd/yy");

    private AuthFraudRules() {
    }

    /**
     * BR-AF-001: WS-AUTH-TS = YY-MM-DD from PA-AUTH-ORIG-DATE, time = 999999999 - PA-AUTH-TIME-9C as HHMMSSmmm,
     * then '000' microseconds, in the X(26) host variable AUTH-TS (blank padded).
     */
    public static String authTsHostVar(String authOrigDate, long authTime9c) {
        String t = String.format("%09d", Math.abs(999_999_999L - authTime9c) % 1_000_000_000L);
        String ts = authOrigDate.substring(0, 2) + "-" + authOrigDate.substring(2, 4) + "-"
                + authOrigDate.substring(4, 6) + " " + t.substring(0, 2) + "." + t.substring(2, 4) + "."
                + t.substring(4, 6) + t.substring(6, 9) + "000";
        return pad(ts, AUTH_TS_HOST_LENGTH);
    }

    /** BR-AF-007/008: WS-SQLCODE PIC +9(06) - leading sign, six digits. */
    public static String sqlCodeEdited(int sqlCode) {
        return (sqlCode < 0 ? "-" : "+") + String.format("%06d", Math.abs((long) sqlCode) % 1_000_000L);
    }

    /** BR-AF-007/008: SQLSTATE X(05) moved to WS-SQLSTATE PIC +9(09) - '+', zero filled, right aligned. */
    public static String sqlStateEdited(String sqlState) {
        String s = sqlState == null ? "" : sqlState.trim();
        if (s.length() > 9) {
            s = s.substring(s.length() - 9);
        }
        return "+" + "0".repeat(9 - s.length()) + s;
    }

    /** BR-AF-008 (+ BR-AF-009 X(50) truncation): insert failed with anything other than -803. */
    public static String systemErrorMessage(int sqlCode, String sqlState) {
        return actMsg(" SYSTEM ERROR DB2: CODE:" + sqlCodeEdited(sqlCode) + ", STATE: " + sqlStateEdited(sqlState));
    }

    /** BR-AF-007 (+ BR-AF-009): FRAUD-UPDATE failed. */
    public static String updateErrorMessage(int sqlCode, String sqlState) {
        return actMsg(" UPDT ERROR DB2: CODE:" + sqlCodeEdited(sqlCode) + ", STATE: " + sqlStateEdited(sqlState));
    }

    /** BR-AF-009: WS-FRD-ACT-MSG is PIC X(50); STRING ... INTO stops at the receiving field length. */
    static String actMsg(String s) {
        return s.length() > ACT_MSG_LENGTH ? s.substring(0, ACT_MSG_LENGTH) : s;
    }

    /** BR-AF-010: CICS FORMATTIME MMDDYY DATESEP -> 'MM/DD/YY', moved to PA-FRAUD-RPT-DATE. */
    public static String fraudRptDate(LocalDate today) {
        return today.format(MMDDYY);
    }

    /** BR-AF-020: COPAUS1C MARK-AUTH-FRAUD - 'F' becomes REMOVE ('R'); anything else becomes REPORT ('F'). */
    public static FraudAction toggle(String currentFlag) {
        return "F".equals(currentFlag) ? FraudAction.REMOVE : FraudAction.REPORT;
    }

    /** BR-AF-022: message after a successful REPL + SYNCPOINT. */
    public static String toggleMessage(FraudAction action) {
        return action == FraudAction.REMOVE ? AUTH_FRAUD_REMOVED : AUTH_MARKED_FRAUD;
    }

    /** BR-AF-024: STRING ' System error while FRAUD Tagging, ROLLBACK||' IMS-RETURN-CODE. */
    public static String fraudTaggingRollback(String imsStatus) {
        return FRAUD_TAGGING_ROLLBACK + imsStatus;
    }

    /**
     * BR-AF-002: MAIN-PARA MOVEs CIPAUDTY -> DCLAUTHFRDS. AUTH_FRAUD = WS-FRD-ACTION, ACCT/CUST from the commarea,
     * MERCHANT-NAME-LEN = LENGTH OF PA-MERCHANT-NAME (always 22). No indicator variables, so no NULLs.
     */
    public static FraudRow toRow(FraudRequest req) {
        AuthorizationDetail a = req.authorization();
        return new FraudRow(
                a.cardNum(), authTsHostVar(a.authOrigDate(), a.authTime9c()), blank(a.authType()),
                blank(a.cardExpiryDate()), blank(a.messageType()), blank(a.messageSource()), blank(a.authIdCode()),
                blank(a.authRespCode()), blank(a.authRespReason()), blank(a.processingCode()),
                amount(a.transactionAmt()), amount(a.approvedAmt()), blank(a.merchantCategoryCode()),
                blank(a.acqrCountryCode()), posEntryMode(a.posEntryMode()), blank(a.merchantId()),
                pad(blank(a.merchantName()), MERCHANT_NAME_LENGTH), blank(a.merchantCity()),
                blank(a.merchantState()), blank(a.merchantZip()), blank(a.transactionId()), blank(a.matchStatus()),
                FraudAction.fromCode(req.action()).code(), req.acctId(), req.custId());
    }

    private static int posEntryMode(String s) {
        return s == null || s.isBlank() ? 0 : Integer.parseInt(s.trim());
    }

    private static BigDecimal amount(BigDecimal b) {
        return b == null ? BigDecimal.ZERO.setScale(2) : b.setScale(2);
    }

    private static String blank(String s) {
        return s == null ? "" : s;
    }

    static String pad(String s, int len) {
        return s.length() >= len ? s : s + " ".repeat(len - s.length());
    }
}
