package com.carddemo.batch.support;

import org.junit.jupiter.api.function.Executable;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Field-level parity assertions. Every assertion message names the file, the record key and the field,
 * and the whole record set is checked with {@code assertAll} so one run reports every mismatch.
 */
public final class FieldAsserts {

    private FieldAsserts() {
    }

    /** A field that is numeric in the copybook: compared with {@code compareTo} and an explicit scale check. */
    public static Executable decimalField(String file, String key, String field, String expected, Object actual,
                                          int scale) {
        return () -> {
            String where = file + " record " + key + " field " + field;
            assertNotNull(actual, where + " is missing from the Java output");
            if (expected.startsWith(JsonRecords.INVALID_PREFIX)) {
                assertEquals(expected, actual, where + " (bytes the COBOL program never assigned)");
                return;
            }
            assertTrue(actual instanceof String, where + " should decode to a decimal string, was " + actual);
            BigDecimal exp = new BigDecimal(expected);
            BigDecimal act;
            try {
                act = new BigDecimal((String) actual);
            } catch (NumberFormatException e) {
                throw new AssertionError(where + " expected " + expected + " but Java produced " + actual);
            }
            assertEquals(0, exp.compareTo(act), where + " expected " + expected + " but was " + actual);
            assertEquals(scale, act.scale(), where + " scale: expected " + scale + " but was " + act.scale());
            assertEquals(exp.scale(), act.scale(), where + " scale differs from the golden text " + expected);
        };
    }

    /** A PIC X field (including dates): exact text, trailing spaces included. */
    public static Executable textField(String file, String key, String field, String expected, Object actual) {
        return () -> {
            String where = file + " record " + key + " field " + field;
            assertNotNull(actual, where + " is missing from the Java output");
            assertEquals(expected, actual, where + " expected '" + expected + "' (" + expected.length()
                    + " chars) but was '" + actual + "' (" + String.valueOf(actual).length() + " chars)");
        };
    }

    /**
     * Compares a Java record map with a golden record map field by field, deciding per field whether it is
     * numeric (golden text parses as a decimal and the field is in {@code numericScale}) or text.
     */
    public static List<Executable> record(String file, String key, Map<String, Object> golden,
                                          Map<String, Object> actual, Map<String, Integer> numericScale) {
        List<Executable> checks = new ArrayList<>();
        for (Map.Entry<String, Object> e : golden.entrySet()) {
            String field = e.getKey();
            Object exp = e.getValue();
            Object act = actual.get(field);
            if (exp instanceof List) {
                @SuppressWarnings("unchecked")
                List<Map<String, Object>> expOcc = (List<Map<String, Object>>) exp;
                checks.add(() -> assertTrue(act instanceof List,
                        file + " record " + key + " field " + field + " should be an OCCURS list"));
                if (act instanceof List) {
                    @SuppressWarnings("unchecked")
                    List<Map<String, Object>> actOcc = (List<Map<String, Object>>) act;
                    checks.add(() -> assertEquals(expOcc.size(), actOcc.size(),
                            file + " record " + key + " field " + field + " OCCURS count"));
                    for (int i = 0; i < Math.min(expOcc.size(), actOcc.size()); i++) {
                        checks.addAll(record(file, key, expOcc.get(i), actOcc.get(i), numericScale, field + "(" + (i + 1) + ")."));
                    }
                }
            } else if (exp instanceof Integer) {
                checks.add(() -> assertEquals(exp, act, file + " record " + key + " field " + field));
            } else if (numericScale.containsKey(field)) {
                checks.add(decimalField(file, key, field, (String) exp, act, numericScale.get(field)));
            } else {
                checks.add(textField(file, key, field, (String) exp, act));
            }
        }
        for (String field : actual.keySet()) {
            if (!golden.containsKey(field)) {
                checks.add(() -> assertTrue(false, file + " record " + key + " field " + field
                        + " is produced by Java but absent from the golden"));
            }
        }
        return checks;
    }

    private static List<Executable> record(String file, String key, Map<String, Object> golden,
                                           Map<String, Object> actual, Map<String, Integer> numericScale,
                                           String prefix) {
        List<Executable> checks = new ArrayList<>();
        for (Map.Entry<String, Object> e : golden.entrySet()) {
            String field = prefix + e.getKey();
            Object act = actual.get(e.getKey());
            if (numericScale.containsKey(e.getKey())) {
                checks.add(decimalField(file, key, field, (String) e.getValue(), act, numericScale.get(e.getKey())));
            } else {
                checks.add(textField(file, key, field, (String) e.getValue(), act));
            }
        }
        return checks;
    }

    /** Runs the per-record checks of a whole file as one assertAll, after checking the record count. */
    public static void recordSet(String file, String keyField, List<Map<String, Object>> golden,
                                 List<Map<String, Object>> actual, Map<String, Integer> numericScale) {
        assertEquals(golden.size(), actual.size(), file + " record count");
        List<Executable> checks = new ArrayList<>();
        for (int i = 0; i < golden.size(); i++) {
            Map<String, Object> g = golden.get(i);
            Map<String, Object> a = actual.get(i);
            String key = keyField + "=" + g.get(keyField) + " (#" + (i + 1) + ")";
            checks.addAll(record(file, key, g, a, numericScale));
        }
        assertAll(file + " field-level parity", checks);
    }
}
