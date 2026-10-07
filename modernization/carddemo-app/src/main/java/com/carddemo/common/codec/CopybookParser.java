package com.carddemo.common.codec;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Optional;
import java.util.Set;

/**
 * Parses the data-description entries of a fixed-format copybook into {@link Field} trees.
 *
 * <p>Handles sequence/indicator areas (columns 1-7 and 73-80), comment lines, tabs (one column, as GnuCOBOL
 * {@code -ftab-width=1} would need for {@code CUSTREC}), levels 01-49 and 77, {@code PIC}, {@code USAGE},
 * {@code REDEFINES}, {@code OCCURS [n TO] m [DEPENDING ON]}, and skips level 88/66 entries and {@code VALUE},
 * {@code SYNC}, {@code JUSTIFIED}, {@code BLANK WHEN ZERO}, {@code INDEXED BY}, {@code KEY} clauses.
 */
final class CopybookParser {

    private static final Set<String> CLAUSE_KEYWORDS = Set.of("REDEFINES", "PIC", "PICTURE", "USAGE", "OCCURS",
            "VALUE", "VALUES", "SIGN", "SYNC", "SYNCHRONIZED", "JUST", "JUSTIFIED", "BLANK", "GLOBAL", "EXTERNAL",
            "INDEXED", "ASCENDING", "DESCENDING", "DEPENDING");

    private static final class Node {
        final int level;
        final String name;
        String picture;
        Usage usage;
        int occurs;
        String dependingOn;
        String redefines;
        final List<Node> children = new ArrayList<>();

        Node(int level, String name) {
            this.level = level;
            this.name = name;
        }
    }

    private CopybookParser() {
    }

    static List<RecordLayout> parse(String copybookName, String source) {
        Node root = new Node(0, copybookName);
        List<Node> stack = new ArrayList<>(List.of(root));
        for (List<String> statement : statements(source)) {
            Node node = entry(statement);
            if (node == null) {
                continue;
            }
            while (stack.get(stack.size() - 1).level >= node.level) {
                stack.remove(stack.size() - 1);
            }
            stack.get(stack.size() - 1).children.add(node);
            stack.add(node);
        }
        if (root.children.isEmpty()) {
            throw new RecordFormatException(copybookName + ": no data description entries");
        }
        List<Node> records = root.children;
        if (records.get(0).level != 1) {
            Node fragment = new Node(1, copybookName);
            fragment.children.addAll(records);
            records = List.of(fragment);
        }
        List<RecordLayout> layouts = new ArrayList<>(records.size());
        for (Node record : records) {
            layouts.add(new RecordLayout(build(record, 0, Usage.DISPLAY, new int[0], new int[0], List.of())));
        }
        return layouts;
    }

    /** Joins the program-text area of each line and splits it into period-terminated token lists. */
    static List<List<String>> statements(String source) {
        StringBuilder text = new StringBuilder();
        for (String raw : source.split("\r?\n", -1)) {
            String line = raw.replace('\t', ' ');
            if (line.length() < 8) {
                continue;
            }
            char indicator = line.charAt(6);
            if (indicator == '*' || indicator == '/' || indicator == 'D' || indicator == 'd') {
                continue;
            }
            String area = line.substring(7, Math.min(line.length(), 72));
            int inlineComment = area.indexOf("*>");
            if (inlineComment >= 0) {
                area = area.substring(0, inlineComment);
            }
            if (indicator == '-') {
                String continued = area.stripLeading();
                if (!continued.isEmpty() && (continued.charAt(0) == '\'' || continued.charAt(0) == '"')) {
                    continued = continued.substring(1);
                    int end = text.length();
                    while (end > 0 && text.charAt(end - 1) == ' ') {
                        end--;
                    }
                    text.setLength(end);
                }
                text.append(continued);
            } else {
                text.append(' ').append(area);
            }
        }
        return tokenize(text.toString());
    }

    private static List<List<String>> tokenize(String text) {
        List<List<String>> statements = new ArrayList<>();
        List<String> current = new ArrayList<>();
        StringBuilder token = new StringBuilder();
        for (int i = 0; i < text.length(); i++) {
            char c = text.charAt(i);
            boolean nextIsBreak = i + 1 >= text.length() || Character.isWhitespace(text.charAt(i + 1));
            if (c == '\'' || c == '"') {
                int j = i + 1;
                while (j < text.length()) {
                    if (text.charAt(j) == c) {
                        if (j + 1 < text.length() && text.charAt(j + 1) == c) {
                            j += 2;
                            continue;
                        }
                        break;
                    }
                    j++;
                }
                if (j >= text.length()) {
                    throw new RecordFormatException("unterminated literal: " + text.substring(i));
                }
                token.append(text, i, j + 1);
                i = j;
            } else if (Character.isWhitespace(c) || ((c == ',' || c == ';') && nextIsBreak)) {
                flush(token, current);
            } else if (c == '.' && nextIsBreak) {
                flush(token, current);
                if (!current.isEmpty()) {
                    statements.add(current);
                    current = new ArrayList<>();
                }
            } else {
                token.append(c);
            }
        }
        flush(token, current);
        if (!current.isEmpty()) {
            statements.add(current);
        }
        return statements;
    }

    private static void flush(StringBuilder token, List<String> current) {
        if (!token.isEmpty()) {
            current.add(token.toString());
            token.setLength(0);
        }
    }

    private static Node entry(List<String> tokens) {
        int level;
        try {
            level = Integer.parseInt(tokens.get(0));
        } catch (NumberFormatException e) {
            throw new RecordFormatException("not a data description entry: " + String.join(" ", tokens), e);
        }
        if (level == 88 || level == 66) {
            return null;
        }
        if (level == 77) {
            level = 1;
        }
        if (level < 1 || level > 49) {
            throw new RecordFormatException("invalid level " + tokens.get(0) + " in " + String.join(" ", tokens));
        }
        int i = 1;
        String name = "FILLER";
        if (i < tokens.size() && !isKeyword(tokens.get(i))) {
            name = tokens.get(i++);
        }
        Node node = new Node(level, name.toUpperCase(Locale.ROOT));
        while (i < tokens.size()) {
            String word = tokens.get(i++).toUpperCase(Locale.ROOT);
            switch (word) {
                case "REDEFINES" -> node.redefines = next(tokens, i++).toUpperCase(Locale.ROOT);
                case "PIC", "PICTURE" -> {
                    i = skip(tokens, i, "IS");
                    node.picture = next(tokens, i++);
                }
                case "USAGE" -> {
                    i = skip(tokens, i, "IS");
                    String usage = next(tokens, i++);
                    node.usage = Usage.fromKeyword(usage)
                            .orElseThrow(() -> new RecordFormatException("unknown USAGE " + usage));
                }
                case "OCCURS" -> {
                    node.occurs = number(next(tokens, i++));
                    if (i < tokens.size() && tokens.get(i).equalsIgnoreCase("TO")) {
                        node.occurs = number(next(tokens, i + 1));
                        i += 2;
                    }
                    i = skip(tokens, i, "TIMES");
                }
                case "DEPENDING" -> {
                    i = skip(tokens, i, "ON");
                    node.dependingOn = next(tokens, i++).toUpperCase(Locale.ROOT);
                    while (i + 1 < tokens.size() && (tokens.get(i).equalsIgnoreCase("OF")
                            || tokens.get(i).equalsIgnoreCase("IN"))) {
                        i += 2;
                    }
                }
                case "VALUE", "VALUES", "INDEXED", "ASCENDING", "DESCENDING" -> {
                    while (i < tokens.size() && !isKeyword(tokens.get(i))) {
                        i++;
                    }
                }
                case "SIGN" -> {
                    i = skip(tokens, i, "IS");
                    String position = next(tokens, i++).toUpperCase(Locale.ROOT);
                    if (!position.equals("TRAILING") || (i < tokens.size()
                            && tokens.get(i).equalsIgnoreCase("SEPARATE"))) {
                        throw new RecordFormatException("SIGN " + position + " (SEPARATE) is not supported: "
                                + String.join(" ", tokens));
                    }
                }
                case "SYNC", "SYNCHRONIZED", "JUST", "JUSTIFIED", "GLOBAL", "EXTERNAL", "IS", "LEFT", "RIGHT" -> {
                    // no effect on the byte layout of unaligned records
                }
                case "BLANK" -> {
                    i = skip(tokens, i, "WHEN");
                    i++;
                }
                default -> {
                    Optional<Usage> usage = Usage.fromKeyword(word);
                    if (usage.isEmpty()) {
                        throw new RecordFormatException("unexpected '" + word + "' in " + String.join(" ", tokens));
                    }
                    node.usage = usage.get();
                }
            }
        }
        return node;
    }

    private static Field build(Node n, int offset, Usage inherited, int[] strides, int[] counts,
                               List<String> ancestors) {
        Usage usage = n.usage != null ? n.usage : inherited;
        int size = sizeOf(n, inherited);
        int[] s = strides;
        int[] c = counts;
        if (n.occurs > 0) {
            s = append(strides, size);
            c = append(counts, n.occurs);
        }
        if (n.picture != null) {
            if (!n.children.isEmpty()) {
                throw new RecordFormatException(n.name + ": a group item cannot have a PIC");
            }
            return new Field(n.name, n.level, offset, size, Picture.parse(n.picture), usage, n.occurs,
                    n.dependingOn, n.redefines, List.of(), s, c, ancestors);
        }
        List<String> childAncestors = new ArrayList<>(ancestors.size() + 1);
        childAncestors.add(n.name);
        childAncestors.addAll(ancestors);
        List<Field> children = new ArrayList<>();
        int cursor = offset;
        for (Node child : n.children) {
            int start = cursor;
            if (child.redefines != null) {
                start = children.stream().filter(f -> f.name().equals(child.redefines)).findFirst()
                        .orElseThrow(() -> new RecordFormatException(child.name + " REDEFINES unknown item "
                                + child.redefines)).offset();
            }
            Field f = build(child, start, usage, s, c, childAncestors);
            children.add(f);
            if (child.redefines == null) {
                cursor = start + f.totalSize();
            }
        }
        return new Field(n.name, n.level, offset, size, null, usage, n.occurs, n.dependingOn, n.redefines,
                children, s, c, ancestors);
    }

    private static int sizeOf(Node n, Usage inherited) {
        Usage usage = n.usage != null ? n.usage : inherited;
        if (n.picture == null) {
            if (n.children.isEmpty()) {
                throw new RecordFormatException(n.name + ": elementary item without PIC");
            }
            int cursor = 0;
            int end = 0;
            List<String> names = new ArrayList<>();
            List<Integer> starts = new ArrayList<>();
            for (Node child : n.children) {
                int start = cursor;
                if (child.redefines != null) {
                    int idx = names.indexOf(child.redefines);
                    if (idx < 0) {
                        throw new RecordFormatException(child.name + " REDEFINES unknown item " + child.redefines);
                    }
                    start = starts.get(idx);
                }
                int childEnd = start + sizeOf(child, usage) * Math.max(child.occurs, 1);
                if (child.redefines == null) {
                    cursor = childEnd;
                }
                end = Math.max(end, childEnd);
                names.add(child.name);
                starts.add(start);
            }
            return end;
        }
        Picture picture = Picture.parse(n.picture);
        return switch (usage) {
            case DISPLAY -> picture.size();
            case BINARY -> CobolNumeric.binaryLength(requireNumeric(n, picture).digits());
            case PACKED -> CobolNumeric.packedLength(requireNumeric(n, picture).digits());
        };
    }

    private static Picture requireNumeric(Node n, Picture picture) {
        if (!picture.isNumeric()) {
            throw new RecordFormatException(n.name + ": " + n.usage + " needs a numeric PIC, got " + picture.text());
        }
        return picture;
    }

    private static boolean isKeyword(String token) {
        String word = token.toUpperCase(Locale.ROOT);
        if (CLAUSE_KEYWORDS.contains(word)) {
            return true;
        }
        try {
            return Usage.fromKeyword(word).isPresent();
        } catch (RecordFormatException unsupported) {
            return true;
        }
    }

    private static String next(List<String> tokens, int i) {
        if (i >= tokens.size()) {
            throw new RecordFormatException("incomplete entry: " + String.join(" ", tokens));
        }
        return tokens.get(i);
    }

    private static int skip(List<String> tokens, int i, String optional) {
        return i < tokens.size() && tokens.get(i).equalsIgnoreCase(optional) ? i + 1 : i;
    }

    private static int number(String token) {
        try {
            return Integer.parseInt(token);
        } catch (NumberFormatException e) {
            throw new RecordFormatException("expected an integer, got " + token, e);
        }
    }

    private static int[] append(int[] values, int value) {
        int[] out = java.util.Arrays.copyOf(values, values.length + 1);
        out[values.length] = value;
        return out;
    }
}
