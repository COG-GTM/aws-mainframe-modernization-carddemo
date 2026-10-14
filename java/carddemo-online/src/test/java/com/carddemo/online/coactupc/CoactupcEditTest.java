package com.carddemo.online.coactupc;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.runtime.cics.CicsContext;
import com.carddemo.runtime.cics.SystemInfo;
import com.carddemo.runtime.cobol.PicX;
import com.carddemo.runtime.vsam.FileControl;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Unit tests for every COACTUPC edit paragraph (1210-1280) and the CSUTLDPY date paragraphs it COPYs. Messages are
 * the STRING statements in app/cbl/COACTUPC.cbl (line numbers per test) and app/cpy/CSUTLDPY.cpy.
 * Flags: LOW-VALUES = valid, '0' = not ok, 'B' = blank.
 */
class CoactupcEditTest {

    private static final Clock CLOCK = Clock.fixed(Instant.parse("2024-03-05T14:07:09.25Z"), ZoneOffset.UTC);
    private static final char OK = Coactupc.LOW;

    private Coactupc.Task t;

    @BeforeEach
    void newTask() {
        t = new Coactupc.Task(CicsContext.builder().transid("CAUP").fileControl(new FileControl()).clock(CLOCK)
                .systemInfo(SystemInfo.defaults()).build());
        t.wsReturnMsg = PicX.spaces(75);
        t.inputFlag = '0';
    }

    private void name(String n) {
        t.editVariableName = PicX.of(n, 25);
    }

    private String msg() {
        return PicX.rtrim(t.wsReturnMsg);
    }

    private static String n(String s) {
        return s == null ? "" : s;
    }

    // ----------------------------------------------------------------- 1210

    @ParameterizedTest
    @CsvSource(nullValues = "NULL", value = {
        "'           ', Account number not provided, ' '",
        "12345678901, '', 1",
        "1234567890a, Account Number if supplied must be a 11 digit Non-Zero Number, 0",
        "00000000000, Account Number if supplied must be a 11 digit Non-Zero Number, 0",
        "'1234567890 ', Account Number if supplied must be a 11 digit Non-Zero Number, 0"})
    void editAccount1210(String acct, String expected, char filter) {
        // lines 1783-1820
        t.ccAcctId = PicX.of(acct, 11);
        t.editAccount1210();
        assertThat(msg()).isEqualTo(n(expected));
        assertThat(t.acctFilterFlag).isEqualTo(filter);
        assertThat(t.inputError()).isEqualTo(filter != '1');
    }

    // ----------------------------------------------------------------- 1215-1245

    @ParameterizedTest
    @CsvSource({"'', B, Group must be supplied.", "'   ', B, Group must be supplied.", "x, L, ''"})
    void editMandatory1215(String value, char flag, String expected) {
        // lines 1823-1853
        name("Group");
        t.setAlphanum(value, 10);
        t.editMandatory1215();
        assertThat(t.mandatoryFlag).isEqualTo(flag == 'L' ? OK : flag);
        assertThat(msg()).isEqualTo(expected);
    }

    @ParameterizedTest
    @CsvSource({"Y, Y, ''", "N, N, ''", "' ', B, Account Status must be supplied.",
        "'0', B, Account Status must be supplied.", "y, 0, Account Status must be Y or N.",
        "X, 0, Account Status must be Y or N."})
    void editYesno1220(char value, char flag, String expected) {
        // lines 1856-1888
        name("Account Status");
        t.editYesNo = value;
        t.editYesno1220();
        assertThat(t.editYesNo).isEqualTo(flag);
        assertThat(msg()).isEqualTo(expected);
    }

    @ParameterizedTest
    @CsvSource({"Mary Ann, L, ''", "'', B, First Name must be supplied.",
        "Mary1, 0, First Name can have alphabets only.", "O'Neil, 0, First Name can have alphabets only."})
    void editAlphaReqd1225(String value, char flag, String expected) {
        // lines 1891-1940
        name("First Name");
        t.setAlphanum(value, 25);
        t.editAlphaReqd1225();
        assertThat(t.alphaFlag).isEqualTo(flag == 'L' ? OK : flag);
        assertThat(msg()).isEqualTo(expected);
    }

    @ParameterizedTest
    @CsvSource({"123 Main St, L, ''", "'', B, Address Line 1 must be supplied.",
        "'Apt. 802', 0, Address Line 1 can have numbers or alphabets only."})
    void editAlphanumReqd1230(String value, char flag, String expected) {
        // lines 1943-1993
        name("Address Line 1");
        t.setAlphanum(value, 50);
        t.editAlphanumReqd1230();
        assertThat(t.alphanumFlag).isEqualTo(flag == 'L' ? OK : flag);
        assertThat(msg()).isEqualTo(expected);
    }

    @ParameterizedTest
    @CsvSource({"Ann, L, ''", "'', L, ''", "A-B, 0, Middle Name can have alphabets only."})
    void editAlphaOpt1235(String value, char flag, String expected) {
        // lines 1996-2044: blank is accepted
        name("Middle Name");
        t.setAlphanum(value, 25);
        t.editAlphaOpt1235();
        assertThat(t.alphaFlag).isEqualTo(flag == 'L' ? OK : flag);
        assertThat(msg()).isEqualTo(expected);
        assertThat(t.inputError()).isEqualTo(flag == '0');
    }

    @ParameterizedTest
    @CsvSource({"Apt 802, L, ''", "'', L, ''", "'#5', 0, Address Line 2 can have numbers or alphabets only."})
    void editAlphanumOpt1240(String value, char flag, String expected) {
        // lines 2047-2096
        name("Address Line 2");
        t.setAlphanum(value, 50);
        t.editAlphanumOpt1240();
        assertThat(t.alphanumFlag).isEqualTo(flag == 'L' ? OK : flag);
        assertThat(msg()).isEqualTo(expected);
    }

    @ParameterizedTest
    @CsvSource({"0049368437, L, ''", "'', B, EFT Account Id must be supplied.",
        "'12345 7890', 0, EFT Account Id must be all numeric.", "0000000000, 0, EFT Account Id must not be zero."})
    void editNumReqd1245(String value, char flag, String expected) {
        // lines 2099-2160
        name("EFT Account Id");
        t.setAlphanum(value, 10);
        t.editNumReqd1245();
        assertThat(t.alphanumFlag).isEqualTo(flag == 'L' ? OK : flag);
        assertThat(msg()).isEqualTo(expected);
    }

    @ParameterizedTest
    @CsvSource({"'+2,020.00', L, ''", "'-1.5', L, ''", "'$5.00', L, ''", "'', B, Credit Limit must be supplied.",
        "'12a', 0, Credit Limit is not valid", "'1.2.3', 0, Credit Limit is not valid"})
    void editSigned9v2_1250(String value, char flag, String expected) {
        // lines 2163-2195 (TEST-NUMVAL-C)
        name("Credit Limit");
        t.editSignedNumber9v2X = value.isEmpty() ? PicX.lowValues(15) : PicX.of(value, 15);
        t.editSigned9v2_1250();
        assertThat(t.signedNumberFlag).isEqualTo(flag == 'L' ? OK : flag);
        assertThat(msg()).isEqualTo(expected);
    }

    // ----------------------------------------------------------------- 1260

    @ParameterizedTest
    @CsvSource({
        "'(212)555-1234', '', LLL",
        "'(   )   -    ', '', LLL",
        "'(   )555-1234', 'Phone Number 1: Area code must be supplied.', BLL",
        "'(abc)555-1234', 'Phone Number 1: Area code must be A 3 digit number.', 0LL",
        "'(000)555-1234', 'Phone Number 1: Area code cannot be zero', 0LL",
        "'(123)555-1234', 'Phone Number 1: Not valid North America general purpose area code', 0LL",
        "'(212)   -1234', 'Phone Number 1: Prefix code must be supplied.', LBL",
        "'(212)5a5-1234', 'Phone Number 1: Prefix code must be A 3 digit number.', L0L",
        "'(212)000-1234', 'Phone Number 1: Prefix code cannot be zero', L0L",
        "'(212)555-    ', 'Phone Number 1: Line number code must be supplied.', LLB",
        "'(212)555-12x4', 'Phone Number 1: Line number code must be A 4 digit number.', LL0",
        "'(212)555-0000', 'Phone Number 1: Line number code cannot be zero', LL0"})
    void editUsPhoneNum1260(String phone, String expected, String flags) {
        // lines 2223-2419; area codes from CSLKPCDY VALID-GENERAL-PURP-CODE
        name("Phone Number 1");
        t.editUsPhoneNum = PicX.of(phone, 15);
        t.editUsPhoneNum1260();
        assertThat(msg()).isEqualTo(expected);
        for (int i = 0; i < 3; i++) {
            char f = flags.charAt(i);
            assertThat(t.phoneFlags[i]).isEqualTo(f == 'L' ? OK : f);
        }
    }

    // ----------------------------------------------------------------- 1265

    @ParameterizedTest
    @CsvSource({
        "020, 97, 3888, ''", "899, 01, 0001, ''", "001, 01, 0001, ''",
        "000, 12, 3456, 'SSN: First 3 chars must not be zero.'",
        "666, 12, 3456, 'SSN: First 3 chars: should not be 000, 666, or between 900 and 999'",
        "900, 12, 3456, 'SSN: First 3 chars: should not be 000, 666, or between 900 and 999'",
        "999, 12, 3456, 'SSN: First 3 chars: should not be 000, 666, or between 900 and 999'",
        "12a, 12, 3456, 'SSN: First 3 chars must be all numeric.'",
        "123, 00, 3456, 'SSN 4th & 5th chars must not be zero.'",
        "123, 12, 0000, 'SSN Last 4 chars must not be zero.'",
        "123, 12, '', 'SSN Last 4 chars must be supplied.'"})
    void editUsSsn1265(String p1, String p2, String p3, String expected) {
        // lines 2421-2477
        t.s(AcupCommarea.NEW.ssn1, p1);
        t.s(AcupCommarea.NEW.ssn2, p2);
        t.s(AcupCommarea.NEW.ssn3, p3);
        t.editUsSsn1265();
        assertThat(msg()).isEqualTo(expected);
        assertThat(t.inputError()).isEqualTo(!expected.isEmpty());
    }

    // ----------------------------------------------------------------- 1270-1280

    @ParameterizedTest
    @CsvSource({"NY, ''", "PR, ''", "XX, 'State: is not a valid state code'", "ny, 'State: is not a valid state code'"})
    void editUsStateCd1270(String state, String expected) {
        // lines 2479-2494
        name("State");
        t.s(AcupCommarea.NEW.stateCd, state);
        t.editUsStateCd1270();
        assertThat(msg()).isEqualTo(expected);
    }

    @ParameterizedTest
    @CsvSource({"299, true", "300, false", "850, false", "851, true", "000, true", "999, true"})
    void editFicoScore1275(String fico, boolean error) {
        // lines 2518-2534: FICO-RANGE-IS-VALID VALUES 300 THRU 850
        name("FICO Score");
        t.s(AcupCommarea.NEW.ficoScoreX, fico);
        t.editFicoScore1275();
        assertThat(msg()).isEqualTo(error ? "FICO Score: should be between 300 and 850" : "");
    }

    @ParameterizedTest
    @CsvSource({"NY, 10001, ''", "NC, 27601, ''", "NC, 12546, Invalid zip code for state",
        "NY, 90210, Invalid zip code for state"})
    void editUsStateZipCd1280(String state, String zip, String expected) {
        // lines 2496-2516: VALID-US-STATE-ZIP-CD2-COMBO
        t.s(AcupCommarea.NEW.stateCd, state);
        t.s(AcupCommarea.NEW.zip, zip);
        t.editUsStateZipCd1280();
        assertThat(msg()).isEqualTo(expected);
    }

    // ----------------------------------------------------------------- CSUTLDPY

    @ParameterizedTest
    @CsvSource({
        "20240229, ''", "20000229, ''", "19991231, ''", "20250131, ''",
        "'        ', 'Open Date : Year must be supplied.'",
        "abcd0101, 'Open Date must be 4 digit number.'",
        "18991231, 'Open Date : Century is not valid.'",
        "'2024  01', 'Open Date : Month must be supplied.'",
        "20241301, 'Open Date: Month must be a number between 1 and 12.'",
        "20240001, 'Open Date: Month must be a number between 1 and 12.'",
        "'202401  ', 'Open Date : Day must be supplied.'",
        "20240100, 'Open Date:day must be a number between 1 and 31.'",
        "20240132, 'Open Date:day must be a number between 1 and 31.'",
        "20240431, 'Open Date:Cannot have 31 days in this month.'",
        "20240230, 'Open Date:Cannot have 30 days in this month.'",
        "20230229, 'Open Date:Not a leap year.Cannot have 29 days in this month.'",
        "19000229, 'Open Date:Not a leap year.Cannot have 29 days in this month.'"})
    void editDateCcyymmdd(String date, String expected) {
        // CSUTLDPY EDIT-DATE-CCYYMMDD / EDIT-YEAR-CCYY / EDIT-MONTH / EDIT-DAY / EDIT-DAY-MONTH-YEAR
        name("Open Date");
        t.editDate = date;
        t.editDateCcyymmdd();
        assertThat(msg()).isEqualTo(expected);
        assertThat(t.inputError()).isEqualTo(!expected.isEmpty());
    }

    @Test
    void ceedaysEmulation() {
        // CSUTLDPY EDIT-DATE-LE -> CSUTLDTC -> CEEDAYS (emulated, see LIMITATIONS)
        assertThat(Coactupc.Task.ceedays("20240229")).containsExactly("0000", "0000");
        assertThat(Coactupc.Task.ceedays("20230229")).containsExactly("0003", "2508");
        assertThat(Coactupc.Task.ceedays("15821014")).containsExactly("0003", "2513");
        assertThat(Coactupc.Task.ceedays("1582101x")).containsExactly("0003", "2520");
    }

    @ParameterizedTest
    @CsvSource({"20240304, ''", "20240305, 'Date of Birth:cannot be in the future'",
        "20240306, 'Date of Birth:cannot be in the future'"})
    void editDateOfBirth(String date, String expected) {
        // CSUTLDPY EDIT-DATE-OF-BIRTH: CURRENT-DATE (2024-03-05) <= date is "future"
        name("Date of Birth");
        t.editDate = date;
        t.editDateOfBirth();
        assertThat(msg()).isEqualTo(expected);
    }
}
