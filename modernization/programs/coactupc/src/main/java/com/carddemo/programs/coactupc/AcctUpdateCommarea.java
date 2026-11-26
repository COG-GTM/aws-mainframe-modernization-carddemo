package com.carddemo.programs.coactupc;

import com.carddemo.programs.coactupc.support.Cobol;
import com.carddemo.programs.coactupc.support.ZonedDecimal;
import java.math.BigDecimal;

/**
 * WS-THIS-PROGCOMMAREA - the program's private part of the COMMAREA, carried between
 * pseudo-conversational turns after CARDDEMO-COMMAREA: ACUP-CHANGE-ACTION, ACUP-OLD-DETAILS and
 * ACUP-NEW-DETAILS. Kept as one character buffer so that REDEFINES (date parts, phone parts,
 * SSN parts and the S9(10)V99 overlays on X(12) amounts) behave exactly like COBOL storage.
 */
public final class AcctUpdateCommarea {

    /** Old (as fetched) or new (as keyed) details. */
    public enum Side {
        OLD(1), NEW(1 + DETAILS_LENGTH);

        final int base;

        Side(int base) {
            this.base = base;
        }
    }

    public static final int DETAILS_LENGTH = 436;
    public static final int ACCT_DATA_LENGTH = 106;
    public static final int LENGTH = 1 + 2 * DETAILS_LENGTH;

    /** Elementary and REDEFINES items of ACUP-OLD-DETAILS / ACUP-NEW-DETAILS (offset within details). */
    public enum DetailField {
        ACCT_ID_X(0, 11),
        ACTIVE_STATUS(11, 1),
        CURR_BAL(12, 12),
        CREDIT_LIMIT(24, 12),
        CASH_CREDIT_LIMIT(36, 12),
        OPEN_DATE(48, 8), OPEN_YEAR(48, 4), OPEN_MON(52, 2), OPEN_DAY(54, 2),
        EXPIRAION_DATE(56, 8), EXP_YEAR(56, 4), EXP_MON(60, 2), EXP_DAY(62, 2),
        REISSUE_DATE(64, 8), REISSUE_YEAR(64, 4), REISSUE_MON(68, 2), REISSUE_DAY(70, 2),
        CURR_CYC_CREDIT(72, 12),
        CURR_CYC_DEBIT(84, 12),
        GROUP_ID(96, 10),
        CUST_ID_X(106, 9),
        CUST_FIRST_NAME(115, 25),
        CUST_MIDDLE_NAME(140, 25),
        CUST_LAST_NAME(165, 25),
        CUST_ADDR_LINE_1(190, 50),
        CUST_ADDR_LINE_2(240, 50),
        CUST_ADDR_LINE_3(290, 50),
        CUST_ADDR_STATE_CD(340, 2),
        CUST_ADDR_COUNTRY_CD(342, 3),
        CUST_ADDR_ZIP(345, 10),
        CUST_PHONE_NUM_1(355, 15), CUST_PHONE_NUM_1A(356, 3), CUST_PHONE_NUM_1B(360, 3), CUST_PHONE_NUM_1C(364, 4),
        CUST_PHONE_NUM_2(370, 15), CUST_PHONE_NUM_2A(371, 3), CUST_PHONE_NUM_2B(375, 3), CUST_PHONE_NUM_2C(379, 4),
        CUST_SSN_X(385, 9), CUST_SSN_1(385, 3), CUST_SSN_2(388, 2), CUST_SSN_3(390, 4),
        CUST_GOVT_ISSUED_ID(394, 20),
        CUST_DOB_YYYY_MM_DD(414, 8), CUST_DOB_YEAR(414, 4), CUST_DOB_MON(418, 2), CUST_DOB_DAY(420, 2),
        CUST_EFT_ACCOUNT_ID(422, 10),
        CUST_PRI_HOLDER_IND(432, 1),
        CUST_FICO_SCORE_X(433, 3);

        public final int offset;
        public final int length;

        DetailField(int offset, int length) {
            this.offset = offset;
            this.length = length;
        }
    }

    /** ACUP-CHANGE-ACTION values (88-levels). */
    public static final char DETAILS_NOT_FETCHED = Cobol.LOW_VALUE;
    public static final char SHOW_DETAILS = 'S';
    public static final char CHANGES_NOT_OK = 'E';
    public static final char CHANGES_OK_NOT_CONFIRMED = 'N';
    public static final char CHANGES_OKAYED_AND_DONE = 'C';
    public static final char CHANGES_OKAYED_LOCK_ERROR = 'L';
    public static final char CHANGES_OKAYED_BUT_FAILED = 'F';

    private final char[] buffer;

    private AcctUpdateCommarea(char[] buffer) {
        this.buffer = buffer;
    }

    /** Fresh working storage: ACUP-CHANGE-ACTION VALUE LOW-VALUES, the rest without VALUE (LOW-VALUES). */
    public static AcctUpdateCommarea lowValues() {
        return new AcctUpdateCommarea(Cobol.lowValues(LENGTH).toCharArray());
    }

    public static AcctUpdateCommarea parse(String image) {
        String padded = image.length() >= LENGTH ? image.substring(0, LENGTH)
                : image + Cobol.lowValues(LENGTH - image.length());
        return new AcctUpdateCommarea(padded.toCharArray());
    }

    public String toFixedWidth() {
        return new String(buffer);
    }

    public AcctUpdateCommarea copy() {
        return new AcctUpdateCommarea(buffer.clone());
    }

    /** INITIALIZE WS-THIS-PROGCOMMAREA: every elementary alphanumeric item to SPACES. */
    public void initialize() {
        java.util.Arrays.fill(buffer, ' ');
    }

    /** INITIALIZE ACUP-OLD-DETAILS / ACUP-NEW-DETAILS. */
    public void initializeDetails(Side side) {
        java.util.Arrays.fill(buffer, side.base, side.base + DETAILS_LENGTH, ' ');
    }

    /** MOVE LOW-VALUES TO ACUP-OLD-ACCT-DATA (or new). */
    public void setAcctDataLowValues(Side side) {
        java.util.Arrays.fill(buffer, side.base, side.base + ACCT_DATA_LENGTH, Cobol.LOW_VALUE);
    }

    public String get(Side side, DetailField field) {
        int start = side.base + field.offset;
        return new String(buffer, start, field.length);
    }

    /** MOVE alphanumeric TO field (pad with spaces / truncate). */
    public void set(Side side, DetailField field, String value) {
        String v = Cobol.pad(value, field.length);
        v.getChars(0, field.length, buffer, side.base + field.offset);
    }

    /** Value of the PIC S9(10)V99 REDEFINES of an X(12) amount (e.g. ACUP-NEW-CREDIT-LIMIT-N). */
    public BigDecimal amount(Side side, DetailField field) {
        return ZonedDecimal.decode(get(side, field), 2);
    }

    /** COMPUTE / MOVE into the PIC S9(10)V99 REDEFINES (truncating). */
    public void setAmount(Side side, DetailField field, BigDecimal value) {
        set(side, field, ZonedDecimal.encode(value, 10, 2));
    }

    /** Value of an unsigned PIC 9(n) REDEFINES (ACUP-*-ACCT-ID, CUST-ID, CUST-SSN, FICO-SCORE). */
    public long unsigned(Side side, DetailField field) {
        return Cobol.unsignedValue(get(side, field));
    }

    public void setUnsigned(Side side, DetailField field, long value) {
        set(side, field, Cobol.unsignedDisplay(value, field.length));
    }

    public char changeAction() {
        return buffer[0];
    }

    public void setChangeAction(char value) {
        buffer[0] = value;
    }

    /** 88 ACUP-DETAILS-NOT-FETCHED VALUES LOW-VALUES, SPACES. */
    public boolean isDetailsNotFetched() {
        return buffer[0] == Cobol.LOW_VALUE || buffer[0] == ' ';
    }

    public boolean isShowDetails() {
        return buffer[0] == SHOW_DETAILS;
    }

    /** 88 ACUP-CHANGES-MADE VALUES 'E', 'N', 'C', 'L', 'F'. */
    public boolean isChangesMade() {
        return "ENCLF".indexOf(buffer[0]) >= 0 && buffer[0] != Cobol.LOW_VALUE;
    }

    public boolean isChangesNotOk() {
        return buffer[0] == CHANGES_NOT_OK;
    }

    public boolean isChangesOkNotConfirmed() {
        return buffer[0] == CHANGES_OK_NOT_CONFIRMED;
    }

    public boolean isChangesOkayedAndDone() {
        return buffer[0] == CHANGES_OKAYED_AND_DONE;
    }

    /** 88 ACUP-CHANGES-FAILED VALUES 'L', 'F'. */
    public boolean isChangesFailed() {
        return buffer[0] == CHANGES_OKAYED_LOCK_ERROR || buffer[0] == CHANGES_OKAYED_BUT_FAILED;
    }

    public boolean isChangesOkayedLockError() {
        return buffer[0] == CHANGES_OKAYED_LOCK_ERROR;
    }

    public boolean isChangesOkayedButFailed() {
        return buffer[0] == CHANGES_OKAYED_BUT_FAILED;
    }

    /** 88 FICO-RANGE-IS-VALID VALUES 300 THROUGH 850 (on ACUP-NEW-CUST-FICO-SCORE). */
    public boolean isFicoRangeValid() {
        long fico = unsigned(Side.NEW, DetailField.CUST_FICO_SCORE_X);
        return fico >= 300 && fico <= 850;
    }
}
