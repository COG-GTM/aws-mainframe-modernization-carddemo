package com.carddemo.runtime.bms;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Parser for BMS macro source (DFHMSD / DFHMDI / DFHMDF). Handles assembler continuation (non-blank in
 * column 72, continuation text from column 16), quoted INITIAL values spanning lines, doubled quotes and
 * doubled ampersands, and comment lines ('*' in column 1).
 */
public final class BmsParser {

    private BmsParser() {
    }

    public static BmsMapsetDefinition parse(String source) {
        List<String[]> statements = statements(source);
        String mapsetName = null;
        Map<String, String> mapsetOps = Map.of();
        List<BmsMapDefinition> maps = new ArrayList<>();
        String mapName = null;
        Map<String, String> mapOps = null;
        List<BmsFieldDefinition> fields = null;
        for (String[] st : statements) {
            String label = st[0];
            String op = st[1];
            Map<String, String> ops = operands(st[2]);
            switch (op) {
                case "DFHMSD" -> {
                    if (!"FINAL".equals(ops.get("TYPE"))) {
                        mapsetName = label;
                        mapsetOps = ops;
                    }
                    if (mapName != null) {
                        maps.add(map(mapName, mapsetName, mapOps, fields));
                        mapName = null;
                    }
                }
                case "DFHMDI" -> {
                    if (mapName != null) {
                        maps.add(map(mapName, mapsetName, mapOps, fields));
                    }
                    mapName = label;
                    mapOps = ops;
                    fields = new ArrayList<>();
                }
                case "DFHMDF" -> {
                    if (fields == null) {
                        throw new IllegalArgumentException("DFHMDF outside DFHMDI: " + label);
                    }
                    fields.add(field(label, ops));
                }
                default -> {
                    // END and other assembler statements are ignored
                }
            }
        }
        if (mapName != null) {
            maps.add(map(mapName, mapsetName, mapOps, fields));
        }
        return new BmsMapsetDefinition(mapsetName, mapsetOps, List.copyOf(maps));
    }

    private static BmsMapDefinition map(String name, String mapset, Map<String, String> ops, List<BmsFieldDefinition> fields) {
        int[] size = pair(ops.getOrDefault("SIZE", "(24,80)"));
        return new BmsMapDefinition(name, mapset, size[0], size[1], List.copyOf(fields), ops);
    }

    private static BmsFieldDefinition field(String label, Map<String, String> ops) {
        int[] pos = pair(ops.getOrDefault("POS", "(1,1)"));
        int length = Integer.parseInt(ops.getOrDefault("LENGTH", "0"));
        List<String> attrs = new ArrayList<>();
        String attrb = ops.get("ATTRB");
        if (attrb != null) {
            for (String a : stripParens(attrb).split(",")) {
                if (!a.isBlank()) {
                    attrs.add(a.trim());
                }
            }
        }
        String initial = ops.containsKey("INITIAL") ? unquote(ops.get("INITIAL")) : null;
        return new BmsFieldDefinition(label, pos[0], pos[1], length, List.copyOf(attrs), ops.get("COLOR"),
                ops.get("HILIGHT"), initial, ops);
    }

    /** Splits source into logical statements: [label or null, operation, operand text]. */
    static List<String[]> statements(String source) {
        List<String[]> out = new ArrayList<>();
        String[] lines = source.replace("\r", "").split("\n", -1);
        StringBuilder current = null;
        for (String raw : lines) {
            if (current == null && (raw.isBlank() || raw.startsWith("*"))) {
                continue;
            }
            boolean continued = raw.length() >= 72 && raw.charAt(71) != ' ';
            String body = raw.length() > 71 ? raw.substring(0, 71) : raw;
            if (current == null) {
                current = new StringBuilder(continued ? body : body.stripTrailing());
            } else {
                String cont = body.length() > 15 ? body.substring(15) : "";
                if (!insideQuote(current)) {
                    trimTrailing(current);
                    cont = cont.stripLeading();
                }
                current.append(continued ? cont : cont.stripTrailing());
            }
            if (!continued) {
                out.add(split(current.toString()));
                current = null;
            }
        }
        return out;
    }

    private static String[] split(String statement) {
        String label = null;
        String rest = statement;
        if (!statement.isEmpty() && statement.charAt(0) != ' ') {
            int sp = statement.indexOf(' ');
            label = sp < 0 ? statement : statement.substring(0, sp);
            rest = sp < 0 ? "" : statement.substring(sp);
        }
        rest = rest.stripLeading();
        int sp = rest.indexOf(' ');
        String op = sp < 0 ? rest : rest.substring(0, sp);
        String operands = sp < 0 ? "" : rest.substring(sp).trim();
        return new String[] {label, op, operands};
    }

    /** Parses KEY=VALUE,KEY=(A,B),KEY='quoted, text' respecting quotes and parentheses. */
    static Map<String, String> operands(String text) {
        Map<String, String> out = new LinkedHashMap<>();
        int depth = 0;
        boolean quote = false;
        int start = 0;
        for (int i = 0; i <= text.length(); i++) {
            char c = i < text.length() ? text.charAt(i) : ',';
            if (c == '\'' && i < text.length()) {
                if (quote && i + 1 < text.length() && text.charAt(i + 1) == '\'') {
                    i++;
                    continue;
                }
                quote = !quote;
            } else if (!quote && c == '(') {
                depth++;
            } else if (!quote && c == ')') {
                depth--;
            } else if (!quote && depth == 0 && (c == ',' || (c == ' ' && i < text.length()))) {
                String item = text.substring(start, i).trim();
                if (!item.isEmpty()) {
                    int eq = item.indexOf('=');
                    out.put(eq < 0 ? item : item.substring(0, eq), eq < 0 ? "" : item.substring(eq + 1));
                }
                start = i + 1;
                if (c == ' ') {
                    break;
                }
            }
        }
        return out;
    }

    private static boolean insideQuote(CharSequence s) {
        boolean quote = false;
        for (int i = 0; i < s.length(); i++) {
            if (s.charAt(i) == '\'') {
                quote = !quote;
            }
        }
        return quote;
    }

    private static void trimTrailing(StringBuilder sb) {
        int end = sb.length();
        while (end > 0 && sb.charAt(end - 1) == ' ') {
            end--;
        }
        sb.setLength(end);
    }

    private static String unquote(String value) {
        String v = value;
        if (v.length() >= 2 && v.startsWith("'") && v.endsWith("'")) {
            v = v.substring(1, v.length() - 1);
        }
        return v.replace("''", "'").replace("&&", "&");
    }

    private static String stripParens(String value) {
        String v = value.trim();
        if (v.startsWith("(") && v.endsWith(")")) {
            v = v.substring(1, v.length() - 1);
        }
        return v;
    }

    private static int[] pair(String value) {
        String[] parts = stripParens(value).split(",");
        return new int[] {Integer.parseInt(parts[0].trim()), Integer.parseInt(parts[1].trim())};
    }
}
