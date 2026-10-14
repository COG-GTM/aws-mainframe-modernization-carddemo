package com.carddemo.online.coactupc;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.runtime.cics.CicsContext;
import com.carddemo.runtime.cics.SystemInfo;
import com.carddemo.runtime.cobol.PicX;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Unit tests per edit paragraph of COACTUPC (app/cbl/COACTUPC.cbl) and the CSUTLDPY date paragraphs
 * (app/cpy/CSUTLDPY.cpy). Expected messages are the literals of the paragraph, prefixed with the trimmed
 * WS-EDIT-VARIABLE-NAME (STRING ... DELIMITED BY SIZE). Flags: LOW-VALUES = valid, '0' = not ok, 'B' = blank.
 */
class CoactupcParagraphsTest {

    static final char L = PicX.LOW_VALUE;
    Coactupc.Task t;

    @BeforeEach
    void setUp() {
        t = new Coactupc.Task(CicsContext.builder().transid("CAUP").clock(CoactupcTest.CLOCK)
                .systemInfo(SystemInfo.defaults()).build());
        t.initializeCcWorkArea();
        t.initializeMiscStorage();
    }

    void name(String n) {
        t.wsEditVariableName = PicX.of(n, 25);
    }

    String msg() {
        return Coactupc.Task.trim(t.wsReturnMsg);
    }

    String flags(char[] f) {
        return new String(f).replace(L, 'L');
    }

    // ---------------------------------------------------------------- CSUTLDPY EDIT-DATE-CCYYMMDD

    @ParameterizedTest(name = "{0} -> {1} / {2}")
    @CsvSource(delimiter = '|', value = {
        "20240229|LLL|",
        "20000229|LLL|",
        "19000229|000|Open Date:Not a leap year.Cannot have 29 days in this month.",
        "20230229|000|Open Date:Not a leap year.Cannot have 29 days in this month.",
        "20240230|L00|Open Date:Cannot have 30 days in this month.",
        "20240431|L00|Open Date:Cannot have 31 days in this month.",
        "20240131|LLL|",
        "20241301|L0L|Open Date: Month must be a number between 1 and 12.",
        "20240001|L0L|Open Date: Month must be a number between 1 and 12.",
        "20240100|LL0|Open Date:day must be a number between 1 and 31.",
        "20240132|LL0|Open Date:day must be a number between 1 and 31.",
        "18990101|0LL|Open Date : Century is not valid.",
        "21000101|0LL|Open Date : Century is not valid.",
        "20AB0101|0LL|Open Date must be 4 digit number.",
        "'    0101'|BLL|Open Date : Year must be supplied.",
        "'2024    '|LBB|Open Date : Month must be supplied.",
        "'202401  '|LLB|Open Date : Day must be supplied.",
    })
    void editDateCcyymmdd(String date, String expectedFlags, String expectedMsg) {
        name("Open Date");
        t.wsEditDateCcyymmdd = date;
        t.editDateCcyymmdd();
        assertThat(flags(t.wsEditDateFlgs)).isEqualTo(expectedFlags);
        assertThat(msg()).isEqualTo(expectedMsg == null ? "" : expectedMsg);
        assertThat(t.wsInputFlag).isEqualTo(expectedMsg == null ? ' ' : '1');
    }

    @Test
    void editDateNormalisesSingleDigitMonthAndDay() {
        // EDIT-MONTH / EDIT-DAY: COMPUTE WS-EDIT-DATE-MM-N = NUMVAL(' 1') rewrites the parts as '01'
        name("Open Date");
        t.wsEditDateCcyymmdd = "2024 1 5";
        t.editDateCcyymmdd();
        assertThat(t.wsEditDateCcyymmdd).isEqualTo("20240105");
        assertThat(flags(t.wsEditDateFlgs)).isEqualTo("LLL");
        assertThat(t.wsSeverity).isEqualTo("0000");
    }

    @ParameterizedTest
    @CsvSource({"20240304,false", "20240305,true", "20240306,true", "19610608,false"})
    void editDateOfBirth(String dob, boolean future) {
        // EDIT-DATE-OF-BIRTH: INTEGER-OF-DATE(dob) must be < INTEGER-OF-DATE(today = 2024-03-05)
        name("Date of Birth");
        t.wsEditDateCcyymmdd = dob;
        java.util.Arrays.fill(t.wsEditDateFlgs, L);
        t.editDateOfBirth();
        assertThat(msg()).isEqualTo(future ? "Date of Birth:cannot be in the future" : "");
        assertThat(flags(t.wsEditDateFlgs)).isEqualTo(future ? "000" : "LLL");
    }

    // ---------------------------------------------------------------- 1220 / 1225 / 1235 / 1215 / 1245

    @ParameterizedTest
    @CsvSource(delimiter = '|', value = {"Y|Y|", "N|N|", "y|0|Account Status must be Y or N.",
        "X|0|Account Status must be Y or N.", "0|B|Account Status must be supplied.",
        "' '|B|Account Status must be supplied."})
    void editYesno1220(char in, char flag, String expected) {
        name("Account Status");
        t.wsEditYesNo = in;
        t.editYesno1220();
        assertThat(t.wsEditYesNo).isEqualTo(flag);
        assertThat(msg()).isEqualTo(expected == null ? "" : expected);
    }

    void alnum(String v, int len) {
        t.wsEditAlphanumOnly = PicX.of(v, 256);
        t.wsEditAlphanumLength = len;
    }

    @ParameterizedTest
    @CsvSource(delimiter = '|', value = {"Mary Ann|L|", "'   '|B|City must be supplied.",
        "St. Louis|0|City can have alphabets only.", "Area51|0|City can have alphabets only."})
    void editAlphaReqd1225(String v, char flag, String expected) {
        name("City");
        alnum(v, 50);
        t.editAlphaReqd1225();
        assertThat(t.wsEditAlphaOnlyFlags).isEqualTo(flag == 'L' ? L : flag);
        assertThat(msg()).isEqualTo(expected == null ? "" : expected);
    }

    @Test
    void editAlphaReqdTreatsLowValuesAsBlank() {
        name("First Name");
        alnum(PicX.lowValues(25), 25);
        t.editAlphaReqd1225();
        assertThat(t.wsEditAlphaOnlyFlags).isEqualTo('B');
        assertThat(msg()).isEqualTo("First Name must be supplied.");
    }

    @Test
    void editAlphaOpt1235AndAlphanum() {
        name("Middle Name");
        alnum(PicX.lowValues(25), 25);
        t.editAlphaOpt1235();
        assertThat(t.wsEditAlphaOnlyFlags).isEqualTo(L);
        alnum("J.", 25);
        t.editAlphaOpt1235();
        assertThat(msg()).isEqualTo("Middle Name can have alphabets only.");

        setUp();
        name("Govt Id");
        alnum("AB12", 20);
        t.editAlphanumReqd1230();
        assertThat(t.wsEditAlphanumOnlyFlags).isEqualTo(L);
        alnum("AB-12", 20);
        t.editAlphanumOpt1240();
        assertThat(msg()).isEqualTo("Govt Id can have numbers or alphabets only.");
    }

    @Test
    void editMandatory1215() {
        name("Address Line 1");
        alnum("1 Main St.", 50);
        t.editMandatory1215();
        assertThat(t.wsEditMandatoryFlags).isEqualTo(L);
        alnum(PicX.lowValues(50), 50);
        t.editMandatory1215();
        assertThat(t.wsEditMandatoryFlags).isEqualTo('B');
        assertThat(msg()).isEqualTo("Address Line 1 must be supplied.");
    }

    @ParameterizedTest
    @CsvSource(delimiter = '|', value = {"27546|L|", "00000|0|Zip must not be zero.", "2754A|0|Zip must be all numeric.",
        "' 2754'|0|Zip must be all numeric.", "'     '|B|Zip must be supplied.", "27546-1234|L|"})
    void editNumReqd1245(String v, char flag, String expected) {
        // only the first WS-EDIT-ALPHANUM-LENGTH (5) characters are edited
        name("Zip");
        alnum(v, 5);
        t.editNumReqd1245();
        assertThat(t.wsEditAlphanumOnlyFlags).isEqualTo(flag == 'L' ? L : flag);
        assertThat(msg()).isEqualTo(expected == null ? "" : expected);
    }

    // ---------------------------------------------------------------- 1250

    @ParameterizedTest
    @CsvSource(delimiter = '|', value = {"+      1,940.00|L|", "-1234.56|L|", "1234.56-|L|", "$1,000.00|L|",
        "12.50CR|L|", "12.3.4|0|Credit Limit is not valid", "1 000|0|Credit Limit is not valid",
        "+-5|0|Credit Limit is not valid"})
    void editSigned9v2_1250(String v, char flag, String expected) {
        name("Credit Limit");
        t.wsEditSignedNumber9v2X = PicX.of(v, 15);
        t.editSigned9v2_1250();
        assertThat(t.wsFlgSignedNumberEdit).isEqualTo(flag == 'L' ? L : flag);
        assertThat(msg()).isEqualTo(expected == null ? "" : expected);
    }

    @Test
    void editSigned9v2Blank() {
        name("Current Cycle Credit Limit");
        t.wsEditSignedNumber9v2X = PicX.lowValues(15);
        t.editSigned9v2_1250();
        assertThat(t.wsFlgSignedNumberEdit).isEqualTo('B');
        // WS-EDIT-VARIABLE-NAME is X(25): 'Current Cycle Credit Limit' (26) loses its last character
        assertThat(msg()).isEqualTo("Current Cycle Credit Limi must be supplied.");
    }

    // ---------------------------------------------------------------- 1260 phone

    static String phone(String a, String b, String c) {
        return " " + a + " " + b + " " + c + "  ";
    }

    @ParameterizedTest
    @CsvSource(delimiter = '|', value = {
        "212|555|1234|LLL|",
        "000|555|1234|0LL|Phone 1: Area code cannot be zero",
        "21A|555|1234|0LL|Phone 1: Area code must be A 3 digit number.",
        "211|555|1234|0LL|Phone 1: Not valid North America general purpose area code",
        "212|000|1234|L0L|Phone 1: Prefix code cannot be zero",
        "212|5X5|1234|L0L|Phone 1: Prefix code must be A 3 digit number.",
        "212|555|0000|LL0|Phone 1: Line number code cannot be zero",
        "212|555|12 4|LL0|Phone 1: Line number code must be A 4 digit number.",
    })
    void editUsPhoneNum1260(String a, String b, String c, String expectedFlags, String expected) {
        name("Phone 1");
        t.wsEditUsPhoneNum = phone(a, b, c);
        t.editUsPhoneNum1260();
        assertThat(flags(t.wsEditUsPhoneNumFlgs)).isEqualTo(expectedFlags);
        assertThat(msg()).isEqualTo(expected == null ? "" : expected);
    }

    @Test
    void phoneAllBlankIsValidAndPartialIsNot() {
        name("Phone 1");
        t.wsEditUsPhoneNum = phone(PicX.lowValues(3), PicX.lowValues(3), PicX.lowValues(4));
        t.editUsPhoneNum1260();
        assertThat(flags(t.wsEditUsPhoneNumFlgs)).isEqualTo("LLL");

        t.wsEditUsPhoneNum = phone(PicX.lowValues(3), PicX.lowValues(3), "1234");
        t.editUsPhoneNum1260();
        assertThat(flags(t.wsEditUsPhoneNumFlgs)).isEqualTo("BBL");
        assertThat(msg()).isEqualTo("Phone 1: Area code must be supplied.");
    }

    // ---------------------------------------------------------------- 1265 / 1270 / 1275 / 1280

    @ParameterizedTest
    @CsvSource(delimiter = '|', value = {
        "020|97|3888|LLL|",
        "899|01|0001|LLL|",
        "666|97|3888|0LL|SSN: First 3 chars: should not be 000, 666, or between 900 and 999",
        "900|97|3888|0LL|SSN: First 3 chars: should not be 000, 666, or between 900 and 999",
        "999|97|3888|0LL|SSN: First 3 chars: should not be 000, 666, or between 900 and 999",
        "000|97|3888|0LL|SSN: First 3 chars must not be zero.",
        "020|00|3888|L0L|SSN 4th & 5th chars must not be zero.",
        "020|97|38A8|LL0|SSN Last 4 chars must be all numeric.",
    })
    void editUsSsn1265(String p1, String p2, String p3, String expectedFlags, String expected) {
        t.acup.setX("ACUP-NEW-CUST-SSN-1", p1);
        t.acup.setX("ACUP-NEW-CUST-SSN-2", p2);
        t.acup.setX("ACUP-NEW-CUST-SSN-3", p3);
        t.editUsSsn1265();
        assertThat(flags(t.wsEditUsSsnFlgs)).isEqualTo(expectedFlags);
        assertThat(msg()).isEqualTo(expected == null ? "" : expected);
    }

    @ParameterizedTest
    @CsvSource({"299,true", "300,false", "850,false", "851,true", "001,true"})
    void editFicoScore1275(String fico, boolean error) {
        name("FICO Score");
        t.acup.setX("ACUP-NEW-CUST-FICO-SCORE-X", fico);
        t.editFicoScore1275();
        assertThat(t.flag("FICO-SCORE")).isEqualTo(error ? '0' : ' ');
        assertThat(msg()).isEqualTo(error ? "FICO Score: should be between 300 and 850" : "");
    }

    @ParameterizedTest
    @CsvSource({"NC,false", "PR,false", "nc,true", "AP,true", "XX,true"})
    void editUsStateCd1270(String state, boolean error) {
        // VALID-US-STATE-CODE (CSLKPCDY): 50 states + DC, AS, GU, MP, PR, VI; case-sensitive; no military codes
        name("State");
        t.acup.setX("ACUP-NEW-CUST-ADDR-STATE-CD", state);
        t.editUsStateCd1270();
        assertThat(msg()).isEqualTo(error ? "State: is not a valid state code" : "");
    }

    @ParameterizedTest
    @CsvSource({"NC,27546,false", "NC,12546,true", "CA,90210,false", "AP,96003,false", "NY,90210,true"})
    void editUsStateZipCd1280(String state, String zip, boolean error) {
        t.acup.setX("ACUP-NEW-CUST-ADDR-STATE-CD", state);
        t.acup.setX("ACUP-NEW-CUST-ADDR-ZIP", zip);
        t.editUsStateZipCd1280();
        assertThat(msg()).isEqualTo(error ? "Invalid zip code for state" : "");
        assertThat(t.flag("ZIPCODE")).isEqualTo(error ? '0' : ' ');
    }

    // ---------------------------------------------------------------- 1210

    @ParameterizedTest
    @CsvSource(delimiter = '|', value = {"00000000001|1|", "'           '|' '|Account number not provided",
        "00000000000|0|Account Number if supplied must be a 11 digit Non-Zero Number",
        "0000000001A|0|Account Number if supplied must be a 11 digit Non-Zero Number"})
    void editAccount1210(String acct, char flag, String expected) {
        t.ccAcctId = acct;
        t.editAccount1210();
        assertThat(t.wsEditAcctFlag).isEqualTo(flag);
        assertThat(msg()).isEqualTo(expected == null ? "" : expected);
    }

    @Test
    void displayNumericsOnInvalidData() {
        // Spaces / LOW-VALUES pack to zero digits; a letter keeps its low nibble ('A' = X'C1' -> 1); '+' (X'4E')
        // is a data exception
        assertThat(DisplayNumerics.longValue("  ")).isZero();
        assertThat(DisplayNumerics.longValue("\0\0")).isZero();
        assertThat(DisplayNumerics.longValue("1A")).isEqualTo(11);
        org.assertj.core.api.Assertions.assertThatThrownBy(() -> DisplayNumerics.longValue("+1"))
                .isInstanceOf(DisplayNumerics.DataException.class);
    }
}
