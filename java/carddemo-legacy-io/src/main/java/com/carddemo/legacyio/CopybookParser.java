package com.carddemo.legacyio;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;
import java.util.Locale;
import java.util.Set;

/**
 * Parses the data description entries of a fixed-format copybook (indicator in column 7, code in columns 8-72).
 * Supports PICTURE, USAGE (DISPLAY, COMP/COMP-4/COMP-5/BINARY, COMP-3/PACKED-DECIMAL), OCCURS n, REDEFINES and
 * VALUE; 88-level condition names are skipped. Clauses that change the byte layout in ways the codec does not model
 * (SYNC, OCCURS DEPENDING ON, SIGN SEPARATE, COMP-1/COMP-2, RENAMES) are rejected.
 */
final class CopybookParser {

    private static final Set<String> USAGE_WORDS = Set.of(
            "DISPLAY", "COMP", "COMPUTATIONAL", "COMP-4", "COMPUTATIONAL-4", "COMP-5", "COMPUTATIONAL-5", "BINARY",
            "COMP-3", "COMPUTATIONAL-3", "PACKED-DECIMAL", "COMP-1", "COMPUTATIONAL-1", "COMP-2", "COMPUTATIONAL-2");
    private static final Set<String> CLAUSE_WORDS = Set.of(
            "PIC", "PICTURE", "USAGE", "REDEFINES", "OCCURS", "VALUE", "VALUES", "SIGN", "SYNC", "SYNCHRONIZED",
            "JUST", "JUSTIFIED", "BLANK", "GLOBAL", "EXTERNAL", "INDEXED", "ASCENDING", "DESCENDING", "LEADING",
            "TRAILING");

    private CopybookParser() {
    }

    static List<CopybookItem> parse(String copybook, String source) {
        List<Entry> roots = new ArrayList<>();
        Deque<Entry> stack = new ArrayDeque<>();
        for (List<String> statement : statements(source)) {
            Entry entry = entry(copybook, statement);
            if (entry == null) {
                continue;
            }
            while (!stack.isEmpty() && stack.peek().level >= entry.level) {
                stack.pop();
            }
            if (stack.isEmpty()) {
                roots.add(entry);
            } else {
                Entry parent = stack.peek();
                if (parent.picture != null) {
                    throw new CopybookParseException(copybook, String.join(" ", statement),
                            "elementary item " + parent.name + " cannot have subordinates");
                }
                if (entry.usage == null) {
                    entry.usage = parent.usage;
                }
                parent.children.add(entry);
            }
            stack.push(entry);
        }
        if (roots.isEmpty()) {
            throw new CopybookParseException(copybook, "", "no data description entries");
        }
        boolean allRecords = roots.stream().allMatch(r -> r.level == 1 || r.level == 77);
        if (!allRecords) {
            Entry wrapper = new Entry(1, copybook.toUpperCase(Locale.ROOT));
            wrapper.children.addAll(roots);
            roots = List.of(wrapper);
        }
        return roots.stream().map(e -> e.build(copybook)).toList();
    }

    private static List<List<String>> statements(String source) {
        StringBuilder code = new StringBuilder();
        for (String rawLine : source.split("\r?\n", -1)) {
            String line = rawLine.replace('\t', ' ');
            if (line.length() < 7) {
                continue;
            }
            char indicator = line.charAt(6);
            if (indicator == '*' || indicator == '/') {
                continue;
            }
            code.append(' ').append(line, 7, Math.min(line.length(), 72));
        }
        List<List<String>> statements = new ArrayList<>();
        List<String> current = new ArrayList<>();
        int i = 0;
        String text = code.toString();
        while (i < text.length()) {
            char c = text.charAt(i);
            if (Character.isWhitespace(c)) {
                i++;
                continue;
            }
            int start = i;
            if (c == '\'' || c == '"') {
                i++;
                while (i < text.length()) {
                    if (text.charAt(i) == c) {
                        if (i + 1 < text.length() && text.charAt(i + 1) == c) {
                            i += 2;
                            continue;
                        }
                        break;
                    }
                    i++;
                }
                i++;
            }
            while (i < text.length() && !Character.isWhitespace(text.charAt(i))) {
                i++;
            }
            String token = text.substring(start, Math.min(i, text.length()));
            if (token.endsWith(".")) {
                String body = token.substring(0, token.length() - 1);
                if (!body.isEmpty()) {
                    current.add(body);
                }
                statements.add(current);
                current = new ArrayList<>();
            } else {
                current.add(token);
            }
        }
        if (!current.isEmpty()) {
            statements.add(current);
        }
        return statements;
    }

    private static Entry entry(String copybook, List<String> tokens) {
        String text = String.join(" ", tokens);
        if (tokens.isEmpty()) {
            return null;
        }
        int level;
        try {
            level = Integer.parseInt(tokens.get(0));
        } catch (NumberFormatException e) {
            throw new CopybookParseException(copybook, text, "expected a level number");
        }
        if (level == 88) {
            return null;
        }
        if (level == 66) {
            throw new CopybookParseException(copybook, text, "RENAMES is not supported");
        }
        if (level < 1 || level > 49 && level != 77) {
            throw new CopybookParseException(copybook, text, "invalid level number " + level);
        }
        int i = 1;
        String name = "FILLER";
        if (i < tokens.size() && !isKeyword(upper(tokens.get(i)))) {
            name = upper(tokens.get(i++));
        }
        Entry entry = new Entry(level, name);
        while (i < tokens.size()) {
            String word = upper(tokens.get(i++));
            switch (word) {
                case "PIC", "PICTURE" -> {
                    i = skip(tokens, i, "IS");
                    entry.picture = Picture.parse(require(copybook, text, tokens, i++));
                }
                case "USAGE" -> {
                    i = skip(tokens, i, "IS");
                    entry.usage = usage(copybook, text, upper(require(copybook, text, tokens, i++)));
                }
                case "REDEFINES" -> entry.redefines = upper(require(copybook, text, tokens, i++));
                case "OCCURS" -> {
                    String count = require(copybook, text, tokens, i++);
                    try {
                        entry.occurs = Integer.parseInt(count);
                    } catch (NumberFormatException e) {
                        throw new CopybookParseException(copybook, text, "OCCURS needs a fixed count");
                    }
                    i = skip(tokens, i, "TIMES");
                    if (i < tokens.size() && Set.of("TO", "DEPENDING").contains(upper(tokens.get(i)))) {
                        throw new CopybookParseException(copybook, text, "OCCURS DEPENDING ON is not supported");
                    }
                }
                case "INDEXED" -> {
                    i = skip(tokens, i, "BY");
                    while (i < tokens.size() && !isKeyword(upper(tokens.get(i)))) {
                        i++;
                    }
                }
                case "ASCENDING", "DESCENDING" -> {
                    i = skip(tokens, skip(tokens, i, "KEY"), "IS");
                    while (i < tokens.size() && !isKeyword(upper(tokens.get(i)))) {
                        i++;
                    }
                }
                case "VALUE", "VALUES" -> {
                    i = skip(tokens, skip(tokens, i, "IS"), "ARE");
                    i = skip(tokens, i, "ALL");
                    require(copybook, text, tokens, i++);
                }
                case "SIGN", "LEADING", "TRAILING" -> {
                    if (word.equals("SIGN")) {
                        i = skip(tokens, i, "IS");
                        word = upper(require(copybook, text, tokens, i++));
                    }
                    if (!word.equals("TRAILING") || i < tokens.size() && upper(tokens.get(i)).equals("SEPARATE")) {
                        throw new CopybookParseException(copybook, text, "only an embedded trailing sign is supported");
                    }
                }
                case "JUST", "JUSTIFIED" -> i = skip(tokens, i, "RIGHT");
                case "BLANK" -> {
                    i = skip(tokens, i, "WHEN");
                    require(copybook, text, tokens, i++);
                }
                case "GLOBAL", "EXTERNAL" -> {
                }
                case "SYNC", "SYNCHRONIZED" ->
                        throw new CopybookParseException(copybook, text, "SYNCHRONIZED is not supported");
                default -> {
                    if (!USAGE_WORDS.contains(word)) {
                        throw new CopybookParseException(copybook, text, "unexpected word " + word);
                    }
                    entry.usage = usage(copybook, text, word);
                }
            }
        }
        if (entry.picture != null && entry.usage != null && entry.usage != Usage.DISPLAY
                && !entry.picture.isNumeric()) {
            throw new CopybookParseException(copybook, text, entry.usage + " requires a numeric PICTURE");
        }
        return entry;
    }

    private static Usage usage(String copybook, String text, String word) {
        return switch (word) {
            case "DISPLAY" -> Usage.DISPLAY;
            case "COMP", "COMPUTATIONAL", "COMP-4", "COMPUTATIONAL-4", "COMP-5", "COMPUTATIONAL-5", "BINARY" ->
                    Usage.BINARY;
            case "COMP-3", "COMPUTATIONAL-3", "PACKED-DECIMAL" -> Usage.PACKED_DECIMAL;
            default -> throw new CopybookParseException(copybook, text, "USAGE " + word + " is not supported");
        };
    }

    private static boolean isKeyword(String word) {
        return CLAUSE_WORDS.contains(word) || USAGE_WORDS.contains(word);
    }

    private static int skip(List<String> tokens, int i, String optionalWord) {
        return i < tokens.size() && upper(tokens.get(i)).equals(optionalWord) ? i + 1 : i;
    }

    private static String require(String copybook, String text, List<String> tokens, int i) {
        if (i >= tokens.size()) {
            throw new CopybookParseException(copybook, text, "incomplete clause");
        }
        return tokens.get(i);
    }

    private static String upper(String token) {
        return token.toUpperCase(Locale.ROOT);
    }

    private static final class Entry {
        final int level;
        final String name;
        final List<Entry> children = new ArrayList<>();
        Picture picture;
        Usage usage;
        String redefines;
        int occurs = 1;

        Entry(int level, String name) {
            this.level = level;
            this.name = name;
        }

        CopybookItem build(String copybook) {
            if (picture == null && children.isEmpty()) {
                throw new CopybookParseException(copybook, level + " " + name, "group item without subordinates");
            }
            return new CopybookItem(level, name, picture, usage == null ? Usage.DISPLAY : usage, redefines, occurs,
                    children.stream().map(c -> c.build(copybook)).toList());
        }
    }
}
