package com.carddemo.account.online;

import com.carddemo.account.online.AccountChanges.DateParts;
import com.carddemo.account.online.AccountChanges.PhoneParts;
import com.carddemo.common.codec.CobolNumeric;
import com.carddemo.common.codec.NumvalC;
import com.carddemo.common.date.CsutldpyDateEdit;
import com.carddemo.common.date.CsutldpyDateEdit.Flag;
import com.carddemo.common.online.Cslkpcdy;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

/**
 * {@code COACTUPC} {@code 1200-EDIT-MAP-INPUTS}: {@code 1205-COMPARE-OLD-NEW} and the field editors, in the
 * program's order. Every field is edited; the first failing edit supplies the message ({@code WS-RETURN-MSG-OFF}
 * guard), every failing field is reported.
 */
public final class AccountUpdateEdits {

    public static final String MSG_NO_CHANGES = "No change detected with respect to values fetched.";
    public static final String MSG_INVALID_ZIP_FOR_STATE = "Invalid zip code for state";

    /** One failed edit: the request field and the COBOL message. */
    public record FieldError(String field, String message) {
    }

    private final List<FieldError> errors = new ArrayList<>();

    private AccountUpdateEdits() {
    }

    /** {@code 1205-COMPARE-OLD-NEW}: true when nothing differs from the fetched values. */
    public static boolean noChanges(AccountChanges fetched, AccountChanges typed) {
        return Fields.upperTrim(typed.activeStatus()).equals(Fields.upperTrim(fetched.activeStatus()))
                && sameAmount(typed.currentBalance(), fetched.currentBalance())
                && sameAmount(typed.creditLimit(), fetched.creditLimit())
                && sameAmount(typed.cashCreditLimit(), fetched.cashCreditLimit())
                && typed.openDate().equals(fetched.openDate())
                && typed.expiryDate().equals(fetched.expiryDate())
                && typed.reissueDate().equals(fetched.reissueDate())
                && sameAmount(typed.currentCycleCredit(), fetched.currentCycleCredit())
                && sameAmount(typed.currentCycleDebit(), fetched.currentCycleDebit())
                && sameText(typed.groupId(), fetched.groupId())
                && sameText(typed.firstName(), fetched.firstName())
                && sameText(typed.middleName(), fetched.middleName())
                && sameText(typed.lastName(), fetched.lastName())
                && sameText(typed.addressLine1(), fetched.addressLine1())
                && sameText(typed.addressLine2(), fetched.addressLine2())
                && sameText(typed.city(), fetched.city())
                && sameText(typed.state(), fetched.state())
                && sameText(typed.zip(), fetched.zip())
                && typed.phone1().equals(fetched.phone1())
                && typed.phone2().equals(fetched.phone2())
                && typed.ssn().equals(fetched.ssn())
                && sameText(typed.governmentId(), fetched.governmentId())
                && typed.dateOfBirth().equals(fetched.dateOfBirth())
                && typed.eftAccountId().equals(fetched.eftAccountId())
                && sameText(typed.primaryCardHolder(), fetched.primaryCardHolder())
                && typed.ficoScore().equals(fetched.ficoScore());
    }

    /**
     * The field edits of {@code 1200-EDIT-MAP-INPUTS} for the typed values; {@code country} is the protected
     * {@code ACSCTRY} value (carried from the record), {@code today} is {@code FUNCTION CURRENT-DATE}.
     */
    public static List<FieldError> edit(AccountChanges typed, String country, LocalDate today) {
        AccountUpdateEdits e = new AccountUpdateEdits();
        e.yesNo("activeStatus", "Account Status", typed.activeStatus());
        e.date("openDate", "Open Date", typed.openDate());
        e.signed9v2("creditLimit", "Credit Limit", typed.creditLimit());
        e.date("expiryDate", "Expiry Date", typed.expiryDate());
        e.signed9v2("cashCreditLimit", "Cash Credit Limit", typed.cashCreditLimit());
        e.date("reissueDate", "Reissue Date", typed.reissueDate());
        e.signed9v2("currentBalance", "Current Balance", typed.currentBalance());
        e.signed9v2("currentCycleCredit", "Current Cycle Credit Limit", typed.currentCycleCredit());
        e.signed9v2("currentCycleDebit", "Current Cycle Debit Limit", typed.currentCycleDebit());
        e.ssn(typed.ssn());
        if (e.date("dateOfBirth", "Date of Birth", typed.dateOfBirth())) {
            e.dateOfBirth(typed.dateOfBirth(), today);
        }
        if (e.numericRequired("ficoScore", "FICO Score", typed.ficoScore(), 3)) {
            e.ficoScore(typed.ficoScore());
        }
        e.alphaRequired("firstName", "First Name", typed.firstName(), 25);
        e.alphaOptional("middleName", "Middle Name", typed.middleName(), 25);
        e.alphaRequired("lastName", "Last Name", typed.lastName(), 25);
        e.mandatory("addressLine1", "Address Line 1", typed.addressLine1());
        boolean stateValid = e.alphaRequired("state", "State", typed.state(), 2) && e.stateCode(typed.state());
        boolean zipValid = e.numericRequired("zip", "Zip", typed.zip(), 5);
        e.alphaRequired("city", "City", typed.city(), 50);
        e.alphaRequired("country", "Country", Objects.requireNonNullElse(country, "").strip(), 3);
        e.phone("phone1", "Phone Number 1", typed.phone1());
        e.phone("phone2", "Phone Number 2", typed.phone2());
        e.numericRequired("eftAccountId", "EFT Account Id", typed.eftAccountId(), 10);
        e.yesNo("primaryCardHolder", "Primary Card Holder", typed.primaryCardHolder());
        if (stateValid && zipValid) {
            e.stateZip(typed.state(), typed.zip());
        }
        return List.copyOf(e.errors);
    }

    /** {@code NUMVAL-C} of a signed 9V2 field as {@code S9(10)V99} stores it; empty when not valid. */
    public static Optional<BigDecimal> amount(String text) {
        if (text == null || text.isEmpty()) {
            return Optional.empty();
        }
        return NumvalC.parse(text).map(v -> CobolNumeric.truncate(v, 12, 2, true));
    }

    private static boolean sameAmount(String typed, String fetched) {
        Optional<BigDecimal> t = amount(typed);
        Optional<BigDecimal> f = amount(fetched);
        if (t.isEmpty() || f.isEmpty()) {
            return t.isEmpty() && f.isEmpty() && typed.equals(fetched);
        }
        return t.get().compareTo(f.get()) == 0;
    }

    private static boolean sameText(String typed, String fetched) {
        return Fields.upperTrim(typed).equals(Fields.upperTrim(fetched));
    }

    private void error(String field, String message) {
        errors.add(new FieldError(field, message));
    }

    /** {@code 1220-EDIT-YESNO}. */
    private void yesNo(String field, String label, String value) {
        if (value.isEmpty() || value.equals("0")) {
            error(field, label + " must be supplied.");
        } else if (!value.equals("Y") && !value.equals("N")) {
            error(field, label + " must be Y or N.");
        }
    }

    /** {@code EDIT-DATE-CCYYMMDD} (copybook {@code CSUTLDPY}). */
    private boolean date(String field, String label, DateParts date) {
        CsutldpyDateEdit.Result r = CsutldpyDateEdit.editDateCcyymmdd(label, date.ccyymmdd());
        if (!r.inputError()) {
            return true;
        }
        error(field + "." + part(r), Fields.rightTrim(r.message()));
        return false;
    }

    /** {@code EDIT-DATE-OF-BIRTH}. */
    private void dateOfBirth(DateParts date, LocalDate today) {
        String ccyymmdd = date.year() + date.month() + String.format("%02d", Integer.parseInt(date.day().strip()));
        CsutldpyDateEdit.Result r = CsutldpyDateEdit.editDateOfBirth("Date of Birth", ccyymmdd, today);
        if (r.inputError()) {
            error("dateOfBirth." + part(r), Fields.rightTrim(r.message()));
        }
    }

    private static String part(CsutldpyDateEdit.Result r) {
        if (r.year() != Flag.VALID) {
            return "year";
        }
        return r.month() != Flag.VALID ? "month" : "day";
    }

    /** {@code 1250-EDIT-SIGNED-9V2}. */
    private void signed9v2(String field, String label, String value) {
        if (value.isEmpty()) {
            error(field, label + " must be supplied.");
        } else if (!NumvalC.isValid(value)) {
            error(field, label + " is not valid");
        }
    }

    /** {@code 1265-EDIT-US-SSN}. */
    private void ssn(AccountChanges.SsnParts ssn) {
        if (numericRequired("ssn.part1", "SSN: First 3 chars", ssn.part1(), 3)) {
            int part1 = Integer.parseInt(ssn.part1());
            if (part1 == 0 || part1 == 666 || part1 >= 900) {
                error("ssn.part1", "SSN: First 3 chars: should not be 000, 666, or between 900 and 999");
            }
        }
        numericRequired("ssn.part2", "SSN 4th & 5th chars", ssn.part2(), 2);
        numericRequired("ssn.part3", "SSN Last 4 chars", ssn.part3(), 4);
    }

    /** {@code 1275-EDIT-FICO-SCORE}. */
    private void ficoScore(String value) {
        int score = Integer.parseInt(value);
        if (score < 300 || score > 850) {
            error("ficoScore", "FICO Score: should be between 300 and 850");
        }
    }

    /** {@code 1245-EDIT-NUM-REQD} over the first {@code length} characters. */
    private boolean numericRequired(String field, String label, String value, int length) {
        if (value.isEmpty()) {
            error(field, label + " must be supplied.");
            return false;
        }
        String image = Fields.fit(value, length);
        if (!digits(image)) {
            error(field, label + " must be all numeric.");
            return false;
        }
        if (Long.parseLong(image) == 0) {
            error(field, label + " must not be zero.");
            return false;
        }
        return true;
    }

    /** {@code 1225-EDIT-ALPHA-REQD}: letters and spaces only. */
    private boolean alphaRequired(String field, String label, String value, int length) {
        if (value.isEmpty()) {
            error(field, label + " must be supplied.");
            return false;
        }
        return alphaOnly(field, label, value, length);
    }

    /** {@code 1235-EDIT-ALPHA-OPT}. */
    private void alphaOptional(String field, String label, String value, int length) {
        if (!value.isEmpty()) {
            alphaOnly(field, label, value, length);
        }
    }

    private boolean alphaOnly(String field, String label, String value, int length) {
        String image = Fields.fit(value, length);
        boolean ok = image.chars().allMatch(c -> c == ' ' || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z'));
        if (!ok) {
            error(field, label + " can have alphabets only.");
        }
        return ok;
    }

    /** {@code 1215-EDIT-MANDATORY}. */
    private void mandatory(String field, String label, String value) {
        if (value.isEmpty()) {
            error(field, label + " must be supplied.");
        }
    }

    /** {@code 1270-EDIT-US-STATE-CD}. */
    private boolean stateCode(String state) {
        if (!Cslkpcdy.isUsStateCode(Fields.fit(state, 2))) {
            error("state", "State: is not a valid state code");
            return false;
        }
        return true;
    }

    /** {@code 1280-EDIT-US-STATE-ZIP-CD}: both fields are flagged, the cursor goes to the zip. */
    private void stateZip(String state, String zip) {
        if (!Cslkpcdy.isUsStateZip2Combo(Fields.fit(state, 2) + Fields.fit(zip, 2).substring(0, 2))) {
            error("zip", MSG_INVALID_ZIP_FOR_STATE);
            error("state", MSG_INVALID_ZIP_FOR_STATE);
        }
    }

    /**
     * {@code 1260-EDIT-US-PHONE-NUM}: optional as a whole (all parts blank; the source tests part A twice, which only
     * matters for a literal space-filled part A, impossible after {@code 1100-RECEIVE-MAP}); each part is then
     * required, numeric, non-zero, and the area code must be a general-purpose NANP code.
     */
    private void phone(String field, String label, PhoneParts phone) {
        if (phone.blank()) {
            return;
        }
        String area = Fields.fit(phone.areaCode(), 3);
        if (phone.areaCode().isEmpty()) {
            error(field + ".areaCode", label + ": Area code must be supplied.");
        } else if (!digits(area)) {
            error(field + ".areaCode", label + ": Area code must be A 3 digit number.");
        } else if (Integer.parseInt(area) == 0) {
            error(field + ".areaCode", label + ": Area code cannot be zero");
        } else if (!Cslkpcdy.isGeneralPurposeAreaCode(area)) {
            error(field + ".areaCode", label + ": Not valid North America general purpose area code");
        }
        String prefix = Fields.fit(phone.prefix(), 3);
        if (phone.prefix().isEmpty()) {
            error(field + ".prefix", label + ": Prefix code must be supplied.");
        } else if (!digits(prefix)) {
            error(field + ".prefix", label + ": Prefix code must be A 3 digit number.");
        } else if (Integer.parseInt(prefix) == 0) {
            error(field + ".prefix", label + ": Prefix code cannot be zero");
        }
        String line = Fields.fit(phone.lineNumber(), 4);
        if (phone.lineNumber().isEmpty()) {
            error(field + ".lineNumber", label + ": Line number code must be supplied.");
        } else if (!digits(line)) {
            error(field + ".lineNumber", label + ": Line number code must be A 4 digit number.");
        } else if (Integer.parseInt(line) == 0) {
            error(field + ".lineNumber", label + ": Line number code cannot be zero");
        }
    }

    private static boolean digits(String s) {
        return !s.isEmpty() && s.chars().allMatch(c -> c >= '0' && c <= '9');
    }
}
