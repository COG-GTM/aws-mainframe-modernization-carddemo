package com.carddemo.programs.csutldtc;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.carddemo.programs.csutldtc.copybook.FeedbackCode;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.HexFormat;
import java.util.stream.Stream;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Byte-for-byte equivalence with the real CSUTLDTC.cbl compiled by GnuCOBOL (-std=ibm) against a
 * CEEDAYS stub; regenerate the golden file with src/test/cobol/generate-goldens.sh.
 */
class GoldenMasterTest {

    record Case(String date, String format, int sev, int msg, boolean negative, int rc, String hex) {
        @Override
        public String toString() {
            return date + "|" + format + " sev=" + (negative ? "-" : "") + sev + " msg=" + msg;
        }
    }

    static Stream<Case> cases() throws IOException {
        try (InputStream in = GoldenMasterTest.class.getResourceAsStream("/golden/csutldtc-golden.txt")) {
            String text = new String(in.readAllBytes(), StandardCharsets.US_ASCII);
            return text.lines().filter(l -> !l.isBlank()).map(l -> {
                String[] p = l.split("\\|", -1);
                return new Case(p[0], p[1], Integer.parseInt(p[2]), Integer.parseInt(p[3]),
                        "Y".equals(p[4]), Integer.parseInt(p[5]), p[6]);
            }).toList().stream();
        }
    }

    @ParameterizedTest(name = "{0}")
    @MethodSource("cases")
    void matchesGnuCobolOutput(Case c) {
        int severity = c.negative() ? -c.sev() : c.sev();
        FeedbackCode fc = c.sev() == 0 && c.msg() == 0 ? FeedbackCode.success() : FeedbackCode.ofCondition(severity, c.msg());
        Csutldtc program = new Csutldtc((date, pic) -> new CeedaysResult(0, fc));

        CsutldtcResult result = program.call(c.date(), c.format());

        assertEquals(c.hex(), HexFormat.of().formatHex(result.lsResultBytes()));
        assertEquals(c.rc(), result.returnCode());
    }
}
