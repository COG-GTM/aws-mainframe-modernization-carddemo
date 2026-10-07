package com.carddemo.common.online;

import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * The level-88 lookup tables of copybook {@code CSLKPCDY} (North American area codes, US state codes, state + first two
 * zip digits), read from the copybook itself ({@code copybooks/CSLKPCDY.cpy} on the classpath) so the lists cannot
 * drift from the legacy source.
 */
public final class Cslkpcdy {

    private static final String RESOURCE = "copybooks/CSLKPCDY.cpy";
    private static final Pattern CONDITION = Pattern.compile("88\\s+([A-Z0-9-]+)\\s+VALUES?\\b");
    private static final Pattern LITERAL = Pattern.compile("'([^']*)'");

    private static final Map<String, Set<String>> CONDITIONS = load();

    /** {@code VALID-GENERAL-PURP-CODE}: the area codes {@code 1260-EDIT-US-PHONE-NUM} accepts. */
    public static final Set<String> GENERAL_PURPOSE_AREA_CODES = condition("VALID-GENERAL-PURP-CODE");
    /** {@code VALID-US-STATE-CODE}. */
    public static final Set<String> US_STATE_CODES = condition("VALID-US-STATE-CODE");
    /** {@code VALID-US-STATE-ZIP-CD2-COMBO}: state code followed by the first two zip digits. */
    public static final Set<String> US_STATE_ZIP2_COMBOS = condition("VALID-US-STATE-ZIP-CD2-COMBO");

    private Cslkpcdy() {
    }

    public static boolean isGeneralPurposeAreaCode(String areaCode) {
        return GENERAL_PURPOSE_AREA_CODES.contains(areaCode);
    }

    public static boolean isUsStateCode(String stateCode) {
        return US_STATE_CODES.contains(stateCode);
    }

    public static boolean isUsStateZip2Combo(String stateAndFirstTwoZipDigits) {
        return US_STATE_ZIP2_COMBOS.contains(stateAndFirstTwoZipDigits);
    }

    /** Every level-88 condition of the copybook with its values, in source order. */
    public static Map<String, Set<String>> conditions() {
        return CONDITIONS;
    }

    private static Set<String> condition(String name) {
        Set<String> values = CONDITIONS.get(name);
        if (values == null || values.isEmpty()) {
            throw new IllegalStateException(RESOURCE + " has no values for " + name);
        }
        return values;
    }

    private static Map<String, Set<String>> load() {
        String source;
        try (InputStream in = Cslkpcdy.class.getClassLoader().getResourceAsStream(RESOURCE)) {
            if (in == null) {
                throw new IllegalStateException(RESOURCE + " is not on the classpath");
            }
            source = new String(in.readAllBytes(), StandardCharsets.US_ASCII);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
        Map<String, Set<String>> conditions = new LinkedHashMap<>();
        Set<String> current = null;
        for (String line : source.split("\\R")) {
            if (line.length() > 6 && line.charAt(6) == '*') {
                continue;
            }
            String text = line.length() > 72 ? line.substring(0, 72) : line;
            Matcher condition = CONDITION.matcher(text);
            if (condition.find()) {
                current = new LinkedHashSet<>();
                conditions.put(condition.group(1), current);
            }
            if (current == null) {
                continue;
            }
            Matcher literal = LITERAL.matcher(text);
            while (literal.find()) {
                current.add(literal.group(1));
            }
            if (text.stripTrailing().endsWith(".")) {
                current = null;
            }
        }
        conditions.replaceAll((name, values) -> Set.copyOf(values));
        return Map.copyOf(conditions);
    }
}
