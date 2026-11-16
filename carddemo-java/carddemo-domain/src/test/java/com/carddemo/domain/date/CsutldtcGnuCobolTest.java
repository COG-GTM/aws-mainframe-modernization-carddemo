package com.carddemo.domain.date;

import static org.assertj.core.api.Assertions.assertThat;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.HexFormat;
import java.util.List;
import org.junit.jupiter.api.Test;

/**
 * Checks {@link Csutldtc}'s {@code LS-RESULT} and {@code RETURN-CODE}, byte for byte, against the
 * unmodified {@code app/cbl/CSUTLDTC.cbl} run under GnuCOBOL with every feedback token it tests plus one
 * it does not ({@code WHEN OTHER}). The fixture is produced by {@code src/test/cobol/csutldtc/generate.sh}.
 */
class CsutldtcGnuCobolTest {

    private static final String FIXTURE = "csutldtc-gnucobol.txt";

    @Test
    void messageMatchesCobolForEveryFeedbackToken() throws IOException {
        List<String[]> cases = fixture();
        assertThat(cases).hasSize(20);
        for (String[] c : cases) {
            byte[] token = Arrays.copyOf(HexFormat.of().parseHex(c[0]), 8);
            String expected = new String(HexFormat.of().parseHex(c[3]), StandardCharsets.ISO_8859_1);

            Csutldtc.Result actual = Csutldtc.a000Main(token, c[1], c[2]);

            assertThat(actual.message()).as("LS-RESULT for token %s, date [%s]", c[0], c[1]).isEqualTo(expected);
            assertThat(actual.returnCode()).as("RETURN-CODE for token %s", c[0]).isEqualTo(Integer.parseInt(c[4]));
        }
    }

    @Test
    void everyCsutldtcConditionIsInTheFixture() throws IOException {
        List<String> tokens = fixture().stream().map(c -> c[0].substring(0, 16)).toList();
        for (CeedaysFeedback feedback : CeedaysFeedback.values()) {
            assertThat(tokens).contains(HexFormat.of().withUpperCase().formatHex(feedback.token()));
        }
    }

    private static List<String[]> fixture() throws IOException {
        try (BufferedReader r = new BufferedReader(new InputStreamReader(
                CsutldtcGnuCobolTest.class.getResourceAsStream(FIXTURE), StandardCharsets.US_ASCII))) {
            return r.lines().filter(l -> !l.startsWith("#") && !l.isBlank()).map(l -> l.split("\\|", -1)).toList();
        }
    }
}
