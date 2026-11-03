package com.carddemo.account.flow;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.carddemo.account.validation.AccountUpdateValidator;
import com.carddemo.account.validation.EditRule;
import com.carddemo.testsupport.flow.CobolFlow;
import com.carddemo.testsupport.flow.CobolMessages;
import com.carddemo.testsupport.flow.ScreenKey;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Every COACTUPC 1200-EDIT-MAP-INPUTS message, one PUT each on sample account 1: the screen as fetched
 * with the three corrections of {@link AccountFlowTestSupport#validScreen}, one field broken, and the message
 * expected as the first {@code fieldErrors} entry (where the COBOL puts the cursor). COACTUPC builds the
 * texts with {@code STRING <label> <suffix>}; the suffixes below are the COBOL literals of COACTUPC and
 * CSUTLDPY, checked against the source by {@link #everySuffixIsACobolLiteral}.
 */
class AccountUpdateMessagesFlowTest extends AccountFlowTestSupport {

    record Case(String paragraph, String field, String value, String message) {
        @Override
        public String toString() {
            return paragraph + " " + field + "='" + value + "' -> " + message;
        }
    }

    private static final String SUPPLIED = " must be supplied.";
    private static final String YES_NO = " must be Y or N.";
    private static final String ALPHA = " can have alphabets only.";
    private static final String NUMERIC = " must be all numeric.";
    private static final String NOT_ZERO = " must not be zero.";
    private static final String NOT_VALID = " is not valid";
    private static final List<String> DATE_SUFFIXES = List.of(" : Year must be supplied.", " must be 4 digit number.",
            " : Century is not valid.", " : Month must be supplied.", ": Month must be a number between 1 and 12.",
            " : Day must be supplied.", ":day must be a number between 1 and 31.", ":Cannot have 31 days in this month.",
            ":Cannot have 30 days in this month.", ":Not a leap year.Cannot have 29 days in this month.");
    private static final List<String> BAD_DATES = List.of("-11-20", "20X4-11-20", "2114-11-20", "2014--20",
            "2014-13-20", "2014-11-", "2014-11-32", "2014-11-31", "2014-02-30", "2015-02-29");
    private static final List<String> PHONE_SUFFIXES = List.of(": Area code must be supplied.",
            ": Area code must be A 3 digit number.", ": Area code cannot be zero",
            ": Not valid North America general purpose area code", ": Prefix code must be supplied.",
            ": Prefix code must be A 3 digit number.", ": Prefix code cannot be zero",
            ": Line number code must be supplied.", ": Line number code must be A 4 digit number.",
            ": Line number code cannot be zero");
    private static final List<String> BAD_PHONES = List.of("-555-0100", "2X1-555-0100", "000-555-0100",
            "373-555-0100", "201--0100", "201-5X5-0100", "201-000-0100", "201-555-", "201-555-01X0", "201-555-0000");
    private static final String FUTURE = ":cannot be in the future";
    private static final String FICO = ": should be between 300 and 850";
    private static final String SSN_RESERVED = ": should not be 000, 666, or between 900 and 999";
    private static final String STATE = ": is not a valid state code";
    private static final String ZIP_FOR_STATE = "Invalid zip code for state";

    static List<Case> cases() {
        List<Case> cases = new ArrayList<>();
        yesNo(cases, "activeStatus", "Account Status");
        date(cases, "openDate", "Open Date");
        amount(cases, "creditLimit", "Credit Limit");
        date(cases, "expirationDate", "Expiry Date");
        amount(cases, "cashCreditLimit", "Cash Credit Limit");
        date(cases, "reissueDate", "Reissue Date");
        amount(cases, "currentBalance", "Current Balance");
        amount(cases, "currentCycleCredit", "Current Cycle Credit Limit");
        amount(cases, "currentCycleDebit", "Current Cycle Debit Limit");
        String ssn = "1265-EDIT-US-SSN";
        cases.add(new Case(ssn, "ssn", "-45-6789", "SSN: First 3 chars" + SUPPLIED));
        cases.add(new Case(ssn, "ssn", "1X3-45-6789", "SSN: First 3 chars" + NUMERIC));
        cases.add(new Case(ssn, "ssn", "000-45-6789", "SSN: First 3 chars" + NOT_ZERO));
        cases.add(new Case(ssn, "ssn", "666-45-6789", "SSN: First 3 chars" + SSN_RESERVED));
        cases.add(new Case(ssn, "ssn", "123--6789", "SSN 4th & 5th chars" + SUPPLIED));
        cases.add(new Case(ssn, "ssn", "123-4X-6789", "SSN 4th & 5th chars" + NUMERIC));
        cases.add(new Case(ssn, "ssn", "123-00-6789", "SSN 4th & 5th chars" + NOT_ZERO));
        cases.add(new Case(ssn, "ssn", "123-45-", "SSN Last 4 chars" + SUPPLIED));
        cases.add(new Case(ssn, "ssn", "123-45-67X9", "SSN Last 4 chars" + NUMERIC));
        cases.add(new Case(ssn, "ssn", "123-45-0000", "SSN Last 4 chars" + NOT_ZERO));
        date(cases, "dateOfBirth", "Date of Birth");
        cases.add(new Case("EDIT-DATE-OF-BIRTH", "dateOfBirth", "2099-01-01", "Date of Birth" + FUTURE));
        numeric(cases, "ficoScore", "FICO Score", "7X0", "000");
        cases.add(new Case("1275-EDIT-FICO-SCORE", "ficoScore", "299", "FICO Score" + FICO));
        cases.add(new Case("1275-EDIT-FICO-SCORE", "ficoScore", "851", "FICO Score" + FICO));
        alphaRequired(cases, "firstName", "First Name");
        cases.add(new Case("1235-EDIT-ALPHA-OPT", "middleName", "M4deline", "Middle Name" + ALPHA));
        alphaRequired(cases, "lastName", "Last Name");
        cases.add(new Case("1215-EDIT-MANDATORY", "addressLine1", " ", "Address Line 1" + SUPPLIED));
        alphaRequired(cases, "stateCode", "State");
        cases.add(new Case("1270-EDIT-US-STATE-CD", "stateCode", "XX", "State" + STATE));
        numeric(cases, "zip", "Zip", "2751X", "00000");
        alphaRequired(cases, "city", "City");
        phone(cases, "phoneNumber1", "Phone Number 1");
        phone(cases, "phoneNumber2", "Phone Number 2");
        numeric(cases, "eftAccountId", "EFT Account Id", "00535X1756", "0000000000");
        yesNo(cases, "primaryCardHolder", "Primary Card Holder");
        cases.add(new Case("1280-EDIT-US-STATE-ZIP-CD", "zip", "12546", ZIP_FOR_STATE));
        return cases;
    }

    @ParameterizedTest(name = "{0}")
    @MethodSource("cases")
    @CobolFlow(program = "COACTUPC", paragraph = "1200-EDIT-MAP-INPUTS", key = ScreenKey.PF5)
    void COACTUPC_1200_EDIT_MAP_INPUTS_everyEditMessage(Case c) throws Exception {
        Map<String, Object> body = validScreen(view(1));
        body.put(c.field(), c.value());
        update("00000000001", body)
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[0].field").value(c.field()))
                .andExpect(jsonPath("$.fieldErrors[0].message").value(c.message()));
    }

    /**
     * Country is protected on the CAUP map; 1225-EDIT-ALPHA-REQD runs on the value COACTUPC filled in from
     * CUSTDAT. Its two messages appear only when the stored country is bad, so the test damages it.
     */
    @Test
    @CobolFlow(program = "COACTUPC", paragraph = "1225-EDIT-ALPHA-REQD", key = ScreenKey.PF5)
    void COACTUPC_1225_EDIT_ALPHA_REQD_storedCountryIsEditedToo() throws Exception {
        long customer = jdbc.queryForObject("SELECT cust_id FROM card.card_xref WHERE acct_id = 1", Long.class);
        try {
            for (String[] c : new String[][] {{"", "Country" + SUPPLIED}, {"U5A", "Country" + ALPHA}}) {
                jdbc.update("UPDATE customer.customers SET addr_country_cd = ? WHERE cust_id = ?", c[0], customer);
                update("00000000001", validScreen(view(1)))
                        .andExpect(status().isBadRequest())
                        .andExpect(jsonPath("$.fieldErrors[0].field").value("countryCode"))
                        .andExpect(jsonPath("$.fieldErrors[0].message").value(c[1]));
            }
        } finally {
            jdbc.update("UPDATE customer.customers SET addr_country_cd = 'USA' WHERE cust_id = ?", customer);
        }
    }

    /** Every message of every AccountUpdateValidator.RULES entry (the 25 edits in COBOL order) has a case. */
    @Test
    void everyRuleMessageHasACase() {
        Set<String> tested = Stream.concat(cases().stream().map(Case::message),
                Stream.of("Country" + SUPPLIED, "Country" + ALPHA)).collect(Collectors.toSet());
        Set<String> untested = new TreeSet<>();
        for (EditRule rule : AccountUpdateValidator.RULES) {
            for (String message : rule.messages()) {
                if (!tested.contains(message)) {
                    untested.add(rule.order() + " " + rule.paragraph() + ": " + message);
                }
            }
        }
        assertThat(AccountUpdateValidator.RULES).hasSize(25);
        assertThat(untested).isEmpty();
    }

    /** The message suffixes above are literals in COACTUPC.cbl or the CSUTLDPY copybook it includes. */
    @Test
    void everySuffixIsACobolLiteral() throws IOException {
        String cobol = cobol(CobolMessages.COBOL_DIR.resolve("COACTUPC.cbl"))
                + cobol(CobolMessages.COBOL_DIR.resolveSibling("cpy").resolve("CSUTLDPY.cpy"));
        List<String> suffixes = new ArrayList<>(List.of(SUPPLIED, YES_NO, ALPHA, NUMERIC, NOT_ZERO, NOT_VALID, FUTURE,
                FICO, SSN_RESERVED, STATE, ZIP_FOR_STATE, "SSN: First 3 chars", "SSN 4th & 5th chars", "SSN Last 4 chars"));
        suffixes.addAll(DATE_SUFFIXES);
        suffixes.addAll(PHONE_SUFFIXES);
        assertThat(suffixes).allSatisfy(suffix -> assertThat(cobol).contains("'" + suffix));
    }

    private static String cobol(Path file) throws IOException {
        return Files.readAllLines(file, StandardCharsets.ISO_8859_1).stream()
                .map(line -> line.length() > 7 ? line.substring(7, Math.min(72, line.length())) : "")
                .collect(Collectors.joining("\n"));
    }

    private static void yesNo(List<Case> cases, String field, String label) {
        cases.add(new Case("1220-EDIT-YESNO", field, " ", label + SUPPLIED));
        cases.add(new Case("1220-EDIT-YESNO", field, "X", label + YES_NO));
    }

    private static void amount(List<Case> cases, String field, String label) {
        cases.add(new Case("1250-EDIT-SIGNED-9V2", field, " ", label + SUPPLIED));
        cases.add(new Case("1250-EDIT-SIGNED-9V2", field, "12X.00", label + NOT_VALID));
    }

    private static void numeric(List<Case> cases, String field, String label, String notNumeric, String zero) {
        cases.add(new Case("1245-EDIT-NUM-REQD", field, " ", label + SUPPLIED));
        cases.add(new Case("1245-EDIT-NUM-REQD", field, notNumeric, label + NUMERIC));
        cases.add(new Case("1245-EDIT-NUM-REQD", field, zero, label + NOT_ZERO));
    }

    private static void alphaRequired(List<Case> cases, String field, String label) {
        cases.add(new Case("1225-EDIT-ALPHA-REQD", field, " ", label + SUPPLIED));
        cases.add(new Case("1225-EDIT-ALPHA-REQD", field, "N0", label + ALPHA));
    }

    private static void date(List<Case> cases, String field, String label) {
        for (int i = 0; i < DATE_SUFFIXES.size(); i++) {
            cases.add(new Case("EDIT-DATE-CCYYMMDD", field, BAD_DATES.get(i), label + DATE_SUFFIXES.get(i)));
        }
    }

    private static void phone(List<Case> cases, String field, String label) {
        for (int i = 0; i < PHONE_SUFFIXES.size(); i++) {
            cases.add(new Case("1260-EDIT-US-PHONE-NUM", field, BAD_PHONES.get(i), label + PHONE_SUFFIXES.get(i)));
        }
    }
}
