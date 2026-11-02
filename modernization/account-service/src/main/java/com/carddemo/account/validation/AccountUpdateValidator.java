package com.carddemo.account.validation;

import static com.carddemo.account.validation.CobolEdits.alphabetsOnly;
import static com.carddemo.account.validation.CobolEdits.isNotValid;
import static com.carddemo.account.validation.CobolEdits.mustBeAllNumeric;
import static com.carddemo.account.validation.CobolEdits.mustBeSupplied;
import static com.carddemo.account.validation.CobolEdits.mustBeYesOrNo;
import static com.carddemo.account.validation.CobolEdits.mustNotBeZero;

import com.carddemo.account.api.dto.AccountUpdateRequest;
import com.carddemo.common.api.ApiError.FieldProblem;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

/**
 * The field edits of COACTUPC {@code 1200-EDIT-MAP-INPUTS}, in the order the COBOL performs them.
 * The COBOL keeps only the first failing message ({@code IF WS-RETURN-MSG-OFF}); {@link #validate}
 * returns one problem per failing field in COBOL order, so the first entry carries the message the
 * 3270 screen would have shown. {@link #RULES} lists every edit with all its messages.
 *
 * <p>Not edited by the COBOL (no rule): group id, address line 2, government issued id. Protected on
 * the screen and therefore not in the request: account id, customer id, country (its edit runs on the
 * stored value, as the COBOL edits the protected field it filled in).
 */
public final class AccountUpdateValidator {

    public static final List<EditRule> RULES = List.of(
            rule(1, "activeStatus", "Account Status", "1220-EDIT-YESNO", yesNoMessages("Account Status")),
            rule(2, "openDate", "Open Date", "EDIT-DATE-CCYYMMDD (CSUTLDPY)", CobolEdits.dateMessages("Open Date")),
            rule(3, "creditLimit", "Credit Limit", "1250-EDIT-SIGNED-9V2", amountMessages("Credit Limit")),
            rule(4, "expirationDate", "Expiry Date", "EDIT-DATE-CCYYMMDD (CSUTLDPY)",
                    CobolEdits.dateMessages("Expiry Date")),
            rule(5, "cashCreditLimit", "Cash Credit Limit", "1250-EDIT-SIGNED-9V2",
                    amountMessages("Cash Credit Limit")),
            rule(6, "reissueDate", "Reissue Date", "EDIT-DATE-CCYYMMDD (CSUTLDPY)",
                    CobolEdits.dateMessages("Reissue Date")),
            rule(7, "currentBalance", "Current Balance", "1250-EDIT-SIGNED-9V2", amountMessages("Current Balance")),
            rule(8, "currentCycleCredit", "Current Cycle Credit Limit", "1250-EDIT-SIGNED-9V2",
                    amountMessages("Current Cycle Credit Limit")),
            rule(9, "currentCycleDebit", "Current Cycle Debit Limit", "1250-EDIT-SIGNED-9V2",
                    amountMessages("Current Cycle Debit Limit")),
            rule(10, "ssn", "SSN", "1265-EDIT-US-SSN", CobolEdits.ssnMessages()),
            rule(11, "dateOfBirth", "Date of Birth", "EDIT-DATE-CCYYMMDD, EDIT-DATE-OF-BIRTH (CSUTLDPY)",
                    concat(CobolEdits.dateMessages("Date of Birth"),
                            List.of(CobolEdits.cannotBeInTheFuture("Date of Birth")))),
            rule(12, "ficoScore", "FICO Score", "1245-EDIT-NUM-REQD, 1275-EDIT-FICO-SCORE",
                    concat(numericMessages("FICO Score"), List.of(CobolEdits.FICO_RANGE))),
            rule(13, "firstName", "First Name", "1225-EDIT-ALPHA-REQD", alphaRequiredMessages("First Name")),
            rule(14, "middleName", "Middle Name", "1235-EDIT-ALPHA-OPT", List.of(alphabetsOnly("Middle Name"))),
            rule(15, "lastName", "Last Name", "1225-EDIT-ALPHA-REQD", alphaRequiredMessages("Last Name")),
            rule(16, "addressLine1", "Address Line 1", "1215-EDIT-MANDATORY",
                    List.of(mustBeSupplied("Address Line 1"))),
            rule(17, "stateCode", "State", "1225-EDIT-ALPHA-REQD, 1270-EDIT-US-STATE-CD",
                    concat(alphaRequiredMessages("State"), List.of(CobolEdits.INVALID_STATE))),
            rule(18, "zip", "Zip", "1245-EDIT-NUM-REQD", numericMessages("Zip")),
            rule(19, "city", "City", "1225-EDIT-ALPHA-REQD", alphaRequiredMessages("City")),
            rule(20, "countryCode", "Country", "1225-EDIT-ALPHA-REQD (stored value, field protected)",
                    alphaRequiredMessages("Country")),
            rule(21, "phoneNumber1", "Phone Number 1", "1260-EDIT-US-PHONE-NUM",
                    CobolEdits.phoneMessages("Phone Number 1")),
            rule(22, "phoneNumber2", "Phone Number 2", "1260-EDIT-US-PHONE-NUM",
                    CobolEdits.phoneMessages("Phone Number 2")),
            rule(23, "eftAccountId", "EFT Account Id", "1245-EDIT-NUM-REQD", numericMessages("EFT Account Id")),
            rule(24, "primaryCardHolder", "Primary Card Holder", "1220-EDIT-YESNO",
                    yesNoMessages("Primary Card Holder")),
            rule(25, "zip", "State/Zip", "1280-EDIT-US-STATE-ZIP-CD", List.of(CobolEdits.INVALID_ZIP_FOR_STATE)));

    private AccountUpdateValidator() {
    }

    /**
     * Runs every edit. {@code storedCountry} is the customer's country code (the protected field);
     * {@code today} drives the date-of-birth check. A blank or missing {@code ssn} is not edited (see
     * {@link AccountUpdateRequest#ssn()}).
     */
    public static List<FieldProblem> validate(AccountUpdateRequest r, String storedCountry, LocalDate today) {
        List<FieldProblem> problems = new ArrayList<>();
        add(problems, "activeStatus", CobolEdits.yesNo("Account Status", r.activeStatus()));
        add(problems, "openDate", CobolEdits.date("Open Date", r.openDate()));
        add(problems, "creditLimit", CobolEdits.signedAmount("Credit Limit", r.creditLimit()));
        add(problems, "expirationDate", CobolEdits.date("Expiry Date", r.expirationDate()));
        add(problems, "cashCreditLimit", CobolEdits.signedAmount("Cash Credit Limit", r.cashCreditLimit()));
        add(problems, "reissueDate", CobolEdits.date("Reissue Date", r.reissueDate()));
        add(problems, "currentBalance", CobolEdits.signedAmount("Current Balance", r.currentBalance()));
        add(problems, "currentCycleCredit",
                CobolEdits.signedAmount("Current Cycle Credit Limit", r.currentCycleCredit()));
        add(problems, "currentCycleDebit",
                CobolEdits.signedAmount("Current Cycle Debit Limit", r.currentCycleDebit()));
        if (CobolEdits.normalize(r.ssn()) != null) {
            add(problems, "ssn", CobolEdits.usSsn(r.ssn()));
        }
        add(problems, "dateOfBirth", CobolEdits.dateOfBirth("Date of Birth", r.dateOfBirth(), today));
        String fico = CobolEdits.numericRequired("FICO Score", r.ficoScore(), 3);
        add(problems, "ficoScore", fico != null ? fico : CobolEdits.ficoRange(r.ficoScore()));
        add(problems, "firstName", CobolEdits.alphaRequired("First Name", r.firstName()));
        add(problems, "middleName", CobolEdits.alphaOptional("Middle Name", r.middleName()));
        add(problems, "lastName", CobolEdits.alphaRequired("Last Name", r.lastName()));
        add(problems, "addressLine1", CobolEdits.mandatory("Address Line 1", r.addressLine1()));
        String state = CobolEdits.alphaRequired("State", r.stateCode());
        if (state == null) {
            state = CobolEdits.usStateCode(r.stateCode());
        }
        add(problems, "stateCode", state);
        String zip = CobolEdits.numericRequired("Zip", r.zip(), 5);
        add(problems, "zip", zip);
        add(problems, "city", CobolEdits.alphaRequired("City", r.city()));
        add(problems, "countryCode", CobolEdits.alphaRequired("Country", storedCountry));
        add(problems, "phoneNumber1", CobolEdits.usPhone("Phone Number 1", r.phoneNumber1()));
        add(problems, "phoneNumber2", CobolEdits.usPhone("Phone Number 2", r.phoneNumber2()));
        add(problems, "eftAccountId", CobolEdits.numericRequired("EFT Account Id", r.eftAccountId(), 10));
        add(problems, "primaryCardHolder", CobolEdits.yesNo("Primary Card Holder", r.primaryCardHolder()));
        if (state == null && zip == null) {
            add(problems, "zip", CobolEdits.usStateZip(r.stateCode(), r.zip()));
        }
        return problems;
    }

    private static void add(List<FieldProblem> problems, String field, String message) {
        if (message != null) {
            problems.add(new FieldProblem(field, message));
        }
    }

    private static EditRule rule(int order, String field, String label, String paragraph, List<String> messages) {
        return new EditRule(order, field, label, paragraph, messages);
    }

    private static List<String> yesNoMessages(String label) {
        return List.of(mustBeSupplied(label), mustBeYesOrNo(label));
    }

    private static List<String> amountMessages(String label) {
        return List.of(mustBeSupplied(label), isNotValid(label));
    }

    private static List<String> numericMessages(String label) {
        return List.of(mustBeSupplied(label), mustBeAllNumeric(label), mustNotBeZero(label));
    }

    private static List<String> alphaRequiredMessages(String label) {
        return List.of(mustBeSupplied(label), alphabetsOnly(label));
    }

    private static List<String> concat(List<String> first, List<String> second) {
        return Stream.concat(first.stream(), second.stream()).toList();
    }
}
