package com.carddemo.common.schema;

import com.carddemo.common.codec.Copybook;
import com.carddemo.common.codec.Field;
import com.carddemo.common.codec.RecordLayout;
import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/** Reads {@code db/copybook-column-map.csv}: which column stores each leaf field of the 11 data layouts. */
final class CopybookColumnMap {

    static final String RESOURCE = "db/copybook-column-map.csv";

    /** One CSV line; {@code table}/{@code column} are empty for an item that is not stored (FILLER). */
    record Entry(String copybook, String dataset, String field, String table, String column) {

        boolean stored() {
            return !table.isEmpty();
        }

        String qualifiedColumn() {
            return table + "." + column;
        }
    }

    private CopybookColumnMap() {
    }

    static List<Entry> entries() {
        try (InputStream in = CopybookColumnMap.class.getClassLoader().getResourceAsStream(RESOURCE)) {
            if (in == null) {
                throw new IllegalStateException("missing " + RESOURCE);
            }
            List<Entry> entries = new ArrayList<>();
            boolean header = true;
            for (String line : new String(in.readAllBytes(), StandardCharsets.UTF_8).split("\r?\n")) {
                if (line.isBlank() || line.startsWith("#")) {
                    continue;
                }
                if (header) {
                    header = false;
                    continue;
                }
                String[] cells = line.split(",", -1);
                if (cells.length != 5) {
                    throw new IllegalStateException(RESOURCE + ": expected 5 cells in '" + line + "'");
                }
                entries.add(new Entry(cells[0].trim(), cells[1].trim(), cells[2].trim(), cells[3].trim(),
                        cells[4].trim()));
            }
            return entries;
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    /** Copybook -> its layout, in map order (the 11 VSAM/sequential layouts). */
    static Map<String, RecordLayout> layouts() {
        Map<String, RecordLayout> layouts = new LinkedHashMap<>();
        for (Entry e : entries()) {
            layouts.computeIfAbsent(e.copybook(), Copybook::layout);
        }
        return layouts;
    }

    /** The ADR-0003/ADR-0004 column type for an elementary item, as PostgreSQL's format_type() prints it. */
    static String sqlType(Field f) {
        if (!f.isNumeric()) {
            return "character varying(" + f.size() + ")";
        }
        if (f.scale() > 0) {
            return "numeric(" + f.digits() + "," + f.scale() + ")";
        }
        return f.digits() <= 9 ? "integer" : "bigint";
    }

    /** Compact COBOL PIC: {@code XXXXXXXX} prints as {@code X(8)}, {@code S9999999999V99} as {@code S9(10)V99}. */
    static String pic(Field f) {
        String text = f.picture().text();
        StringBuilder out = new StringBuilder();
        int i = 0;
        while (i < text.length()) {
            int j = i;
            while (j < text.length() && text.charAt(j) == text.charAt(i)) {
                j++;
            }
            int run = j - i;
            out.append(run > 2 ? text.charAt(i) + "(" + run + ")" : text.substring(i, j));
            i = j;
        }
        return out.toString();
    }

    static String usage(Field f) {
        return switch (f.usage()) {
            case DISPLAY -> "DISPLAY";
            case PACKED -> "COMP-3";
            case BINARY -> "COMP";
        };
    }

    static List<Entry> forCopybook(String copybook) {
        return entries().stream().filter(e -> e.copybook().equals(copybook)).toList();
    }
}
