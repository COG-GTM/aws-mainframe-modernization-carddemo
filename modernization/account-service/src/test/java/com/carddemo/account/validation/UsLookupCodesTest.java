package com.carddemo.account.validation;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.common.schema.MigrationDocs;
import java.io.IOException;
import java.nio.file.Files;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.junit.jupiter.api.Test;

/** UsLookupCodes must hold exactly the 88-level values of app/cpy/CSLKPCDY.cpy. */
class UsLookupCodesTest {

    private static final Pattern LEVEL_88 = Pattern.compile("88\\s+([A-Z0-9-]+)\\s+VALUES?");
    private static final Pattern LITERAL = Pattern.compile("'([^']*)'");

    @Test
    void matchesTheCopybook() throws IOException {
        Map<String, Set<String>> values = new HashMap<>();
        String current = null;
        for (String raw : Files.readAllLines(MigrationDocs.copybook("CSLKPCDY"))) {
            String line = raw.length() > 72 ? raw.substring(6, 72) : raw.length() > 6 ? raw.substring(6) : "";
            if (line.startsWith("*")) {
                continue;
            }
            if (line.matches("\\s*01 .*")) {
                current = null;
            }
            Matcher level = LEVEL_88.matcher(line);
            if (level.find()) {
                current = level.group(1);
                values.put(current, new HashSet<>());
            }
            if (current != null) {
                Matcher literal = LITERAL.matcher(line);
                while (literal.find()) {
                    values.get(current).add(literal.group(1));
                }
            }
        }
        assertThat(UsLookupCodes.GENERAL_PURPOSE_AREA_CODES).isEqualTo(values.get("VALID-GENERAL-PURP-CODE"));
        assertThat(UsLookupCodes.STATE_CODES).isEqualTo(values.get("VALID-US-STATE-CODE"));
        assertThat(UsLookupCodes.STATE_ZIP_PREFIXES).isEqualTo(values.get("VALID-US-STATE-ZIP-CD2-COMBO"));
    }
}
