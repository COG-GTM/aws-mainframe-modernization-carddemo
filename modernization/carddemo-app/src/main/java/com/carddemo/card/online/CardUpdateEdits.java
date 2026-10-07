package com.carddemo.card.online;

import com.carddemo.common.online.ScreenInput;
import java.util.ArrayList;
import java.util.List;

/**
 * COCRDUPC edit phase ({@code 1200-EDIT-MAP-INPUTS} once details are fetched): change detection, then one method
 * per edit paragraph in source order. Every paragraph runs so all failing fields are known; the first failure's
 * message is the one shown ({@code IF WS-RETURN-MSG-OFF}).
 */
public final class CardUpdateEdits {

    public static final String NAME_FIELD = "embossedName";
    public static final String STATUS_FIELD = "activeStatus";
    public static final String MONTH_FIELD = "expiryMonth";
    public static final String YEAR_FIELD = "expiryYear";

    public static final String MSG_NO_CHANGES = "No change detected with respect to values fetched.";
    public static final String MSG_NAME_NOT_PROVIDED = "Card name not provided";
    public static final String MSG_NAME_NOT_ALPHA = "Card name can only contain alphabets and spaces";
    public static final String MSG_STATUS_NOT_YES_NO = "Card Active Status must be Y or N";
    public static final String MSG_MONTH_INVALID = "Card expiry month must be between 1 and 12";
    public static final String MSG_YEAR_INVALID = "Invalid card expiry year";

    public record FieldError(String field, String message) {
    }

    private CardUpdateEdits() {
    }

    /**
     * {@code FUNCTION UPPER-CASE(CCUP-NEW-CARDDATA) = FUNCTION UPPER-CASE(CCUP-OLD-CARDDATA)} (R-13), field by field
     * after trimming the BMS padding; {@code *} is low-values (R-9) and a one-digit month is the two-digit month.
     */
    public static boolean noChanges(CardChanges fetched, CardChanges typed) {
        return same(fetched.embossedName(), typed.embossedName())
                && same(fetched.activeStatus(), typed.activeStatus())
                && same(month(fetched.expiryMonth()), month(typed.expiryMonth()))
                && same(fetched.expiryYear(), typed.expiryYear());
    }

    /** {@code 1230}..{@code 1260} in source order (R-14). */
    public static List<FieldError> edit(CardChanges typed) {
        List<FieldError> errors = new ArrayList<>();
        editName(typed.embossedName(), errors);
        editCardStatus(typed.activeStatus(), errors);
        editExpiryMonth(typed.expiryMonth(), errors);
        editExpiryYear(typed.expiryYear(), errors);
        return errors;
    }

    /**
     * {@code 1230-EDIT-NAME}: blank/zeros → not provided; {@code INSPECT ... CONVERTING} A–Z/a–z to spaces must
     * leave nothing but spaces (R-15).
     */
    static void editName(String name, List<FieldError> errors) {
        if (blank(name)) {
            errors.add(new FieldError(NAME_FIELD, MSG_NAME_NOT_PROVIDED));
            return;
        }
        boolean alphaOrSpace = name.chars()
                .allMatch(c -> c == ' ' || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z'));
        if (!alphaOrSpace) {
            errors.add(new FieldError(NAME_FIELD, MSG_NAME_NOT_ALPHA));
        }
    }

    /** {@code 1240-EDIT-CARDSTATUS}: {@code FLG-YES-NO-VALID VALUES 'Y', 'N'} — upper case only (R-16). */
    static void editCardStatus(String status, List<FieldError> errors) {
        String s = status == null ? "" : status.strip();
        if (blank(status) || !(s.equals("Y") || s.equals("N"))) {
            errors.add(new FieldError(STATUS_FIELD, MSG_STATUS_NOT_YES_NO));
        }
    }

    /** {@code 1250-EDIT-EXPIRY-MON}: {@code VALID-MONTH VALUES 1 THRU 12} (R-17). */
    static void editExpiryMonth(String month, List<FieldError> errors) {
        if (blank(month) || !inRange(month, 2, 1, 12)) {
            errors.add(new FieldError(MONTH_FIELD, MSG_MONTH_INVALID));
        }
    }

    /** {@code 1260-EDIT-EXPIRY-YEAR}: {@code VALID-YEAR VALUES 1950 THRU 2099} (R-18). */
    static void editExpiryYear(String year, List<FieldError> errors) {
        if (blank(year) || !inRange(year, 4, 1950, 2099)) {
            errors.add(new FieldError(YEAR_FIELD, MSG_YEAR_INVALID));
        }
    }

    /** The month as stored ({@code MM}): a valid one-digit month gets its leading zero. */
    public static String month(String month) {
        String m = month == null ? "" : month.strip();
        return m.length() == 1 && Character.isDigit(m.charAt(0)) ? "0" + m : m;
    }

    /** {@code EQUAL LOW-VALUES OR SPACES OR ZEROS}, with {@code *} received as low-values (R-9). */
    static boolean blank(String value) {
        String v = value == null ? "" : value.strip();
        return v.isEmpty() || v.equals("*") || v.chars().allMatch(c -> c == '0');
    }

    private static boolean inRange(String value, int width, int low, int high) {
        String v = value.strip();
        if (v.length() > width || !v.chars().allMatch(c -> c >= '0' && c <= '9')) {
            return false;
        }
        int n = Integer.parseInt(v);
        return n >= low && n <= high;
    }

    private static boolean same(String fetched, String typed) {
        return upperTrim(fetched).equals(upperTrim(typed));
    }

    private static String upperTrim(String value) {
        String v = value == null ? "" : value.strip();
        return ScreenInput.upperCase(v.equals("*") ? "" : v);
    }
}
