package com.carddemo.online.coactupc;

import static org.assertj.core.api.Assertions.assertThat;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.junit.jupiter.api.Test;

/** Re-parses app/cpy/CSLKPCDY.cpy and checks the transcribed {@link Cslkpcdy} sets are identical. */
class CslkpcdyTest {

    private static Map<String, Set<String>> parseCopybook() throws Exception {
        Path cpy = Path.of("..", "..", "app", "cpy", "CSLKPCDY.cpy");
        List<String> lines = Files.readAllLines(cpy, java.nio.charset.StandardCharsets.ISO_8859_1);
        Map<String, Set<String>> sets = new HashMap<>();
        Pattern level88 = Pattern.compile("88\\s+([A-Z0-9-]+)\\s+VALUES?");
        Pattern literal = Pattern.compile("'([^']*)'");
        String current = null;
        for (String line : lines) {
            String body = line.length() > 6 ? line.substring(6, Math.min(72, line.length())) : "";
            if (body.trim().startsWith("*")) {
                continue;
            }
            Matcher m = level88.matcher(body);
            if (m.find()) {
                current = m.group(1);
                sets.put(current, new HashSet<>());
            } else if (body.matches(".*\\b01\\s.*")) {
                current = null;
            }
            if (current != null) {
                Matcher l = literal.matcher(body);
                while (l.find()) {
                    sets.get(current).add(l.group(1));
                }
            }
        }
        return sets;
    }

    @Test
    void transcribedSetsMatchTheCopybook() throws Exception {
        Map<String, Set<String>> cpy = parseCopybook();
        assertThat(Cslkpcdy.VALID_PHONE_AREA_CODE).isEqualTo(cpy.get("VALID-PHONE-AREA-CODE"));
        assertThat(Cslkpcdy.VALID_GENERAL_PURP_CODE).isEqualTo(cpy.get("VALID-GENERAL-PURP-CODE"));
        assertThat(Cslkpcdy.VALID_EASY_RECOG_AREA_CODE).isEqualTo(cpy.get("VALID-EASY-RECOG-AREA-CODE"));
        assertThat(Cslkpcdy.VALID_US_STATE_CODE).isEqualTo(cpy.get("VALID-US-STATE-CODE"));
        assertThat(Cslkpcdy.VALID_US_STATE_ZIP_CD2_COMBO).isEqualTo(cpy.get("VALID-US-STATE-ZIP-CD2-COMBO"));
        assertThat(Cslkpcdy.VALID_US_STATE_CODE).hasSize(56).contains("NY", "PR", "DC").doesNotContain("ny", "XX");
    }
}
