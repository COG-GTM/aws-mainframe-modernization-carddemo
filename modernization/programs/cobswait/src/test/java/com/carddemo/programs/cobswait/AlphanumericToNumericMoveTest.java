package com.carddemo.programs.cobswait;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.carddemo.programs.cobswait.copybook.ParmValue;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

class AlphanumericToNumericMoveTest {

    /** Expected values recorded by running COBSWAIT.cbl under GnuCOBOL 3.1.2 (-std=ibm). */
    @ParameterizedTest(name = "GnuCOBOL [{0}] -> {1}")
    @CsvSource(delimiter = '|', ignoreLeadingAndTrailingWhitespace = false, value = {
        "00003600      VALUE IN CENTISECONDS|3600",
        "3600|3600",
        "   36|36",
        "  36  |36",
        "12345678901|12345678",
        "99999999|99999999",
        "00000036|36",
        "ABCD1234|0",
        "-0000100|100",
        " -5|5",
        "- 1|1",
        "+100|100",
        "100-|0",
        "1+|0",
        "+-1|0",
        "+|0",
        "0000.100|0",
        "12.9|12",
        "1.2.3|0",
        ".5|0",
        "12 34|1234",
        "1,000|1000",
        "1,2,3|123",
        ",1|1",
        "1A|0",
        "A1|0",
        "1 2-|0",
        "0|0",
        "''|0",
        "'        '|0",
        "3\t600|3600",
        "\t36|36",
        "-\t1|1",
        ",-3600|0",
        ",+1|0",
        "1,-1|0",
        "-,1|1",
        "' ,1'|1",
        "1\t,2|12",
        "1, 2|12",
        "+,|0",
        ",|0",
        "1.,5|1",
        "1.|1",
        "-.5|0",
    })
    void gnuCobolMatchesObservedRuntime(String sysin, long expected) {
        assertEquals(expected,
                AlphanumericToNumericMove.GNUCOBOL.apply(ParmValue.of(sysin)).centiseconds());
    }

    @ParameterizedTest(name = "IBM [{0}] -> {1}")
    @CsvSource(delimiter = '|', ignoreLeadingAndTrailingWhitespace = false, value = {
        "00003600      VALUE IN CENTISECONDS|3600",
        "00000036|36",
        "99999999|99999999",
        "12345678901|12345678",
        "3600|36000000",
        "   36|36000",
        "''|0",
        "-0000100|100",
        "ABCD1234|12341234",
        "JKLMNOPQ|12345678",
        "STUVWXYZ|23456789",
    })
    void ibmUsesEbcdicDigitNibbles(String sysin, long expected) {
        assertEquals(expected,
                AlphanumericToNumericMove.IBM_ENTERPRISE.apply(ParmValue.of(sysin)).centiseconds());
    }

    @ParameterizedTest(name = "IBM S0C7 on [{0}]")
    @CsvSource(delimiter = '|', ignoreLeadingAndTrailingWhitespace = false, value = {
        "0000.100", "+100", "1,000", "100*"
    })
    void ibmAbendsS0c7OnInvalidDigitNibble(String sysin) {
        DataExceptionAbend abend = assertThrows(DataExceptionAbend.class,
                () -> AlphanumericToNumericMove.IBM_ENTERPRISE.apply(ParmValue.of(sysin)));
        assertTrue(abend.getMessage().startsWith("S0C7 MOVE PARM-VALUE TO MVSWAIT-TIME"));
    }

    @org.junit.jupiter.api.Test
    void ibmAbendMessageShowsEbcdicHexNotRawInput() {
        DataExceptionAbend abend = assertThrows(DataExceptionAbend.class,
                () -> AlphanumericToNumericMove.IBM_ENTERPRISE.apply(ParmValue.of("0000.100")));
        assertEquals("S0C7 MOVE PARM-VALUE TO MVSWAIT-TIME: invalid digit at offset 4 in"
                + " X'F0F0F0F04BF1F0F0'", abend.getMessage());
        DataExceptionAbend esc = assertThrows(DataExceptionAbend.class,
                () -> AlphanumericToNumericMove.IBM_ENTERPRISE.apply(ParmValue.of("\u001b[2J")));
        assertTrue(esc.getMessage().chars().allMatch(c -> c >= 0x20 && c < 0x7f));
    }
}
