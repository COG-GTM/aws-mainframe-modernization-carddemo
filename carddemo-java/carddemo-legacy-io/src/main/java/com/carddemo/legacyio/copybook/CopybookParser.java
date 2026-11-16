package com.carddemo.legacyio.copybook;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

/**
 * Parses the data division subset used by record copybooks: fixed-format source (sequence area, indicator column,
 * columns 8-72), level numbers, {@code PIC}, {@code USAGE}, {@code OCCURS}, {@code REDEFINES}; {@code VALUE} clauses
 * and level-88/66 entries are skipped. Tabs expand to 3 columns, as with {@code cobc -ftab-width=3}.
 */
final class CopybookParser {

    private static final int TAB_WIDTH = 3;
    private static final Set<String> CLAUSE_KEYWORDS = Set.of(
            "PIC", "PICTURE", "USAGE", "OCCURS", "REDEFINES", "VALUE", "VALUES", "COMP", "COMP-3", "COMP-4", "COMP-5",
            "COMPUTATIONAL", "COMPUTATIONAL-3", "COMPUTATIONAL-4", "COMPUTATIONAL-5", "BINARY", "PACKED-DECIMAL",
            "DISPLAY", "SYNC", "SYNCHRONIZED", "JUST", "JUSTIFIED", "BLANK", "SIGN", "EXTERNAL", "GLOBAL");

    private final String copybook;
    private final String source;

    CopybookParser(String copybook, String source) {
        this.copybook = copybook;
        this.source = source;
    }

    List<CopybookItem> parse() {
        List<Entry> roots = buildTree(parseEntries(statements(codeArea(source))));
        List<CopybookItem> records = new ArrayList<>();
        for (Entry root : roots) {
            records.add(resolve(root, 0, Usage.DISPLAY));
        }
        return records;
    }

    private static String codeArea(String text) {
        StringBuilder code = new StringBuilder();
        for (String raw : text.split("\r?\n", -1)) {
            String line = expandTabs(raw);
            if (line.length() <= 6) {
                continue;
            }
            char indicator = line.charAt(6);
            if (indicator == '*' || indicator == '/' || indicator == 'D' || indicator == 'd') {
                continue;
            }
            String area = line.substring(7, Math.min(72, line.length()));
            if (indicator == '-') {
                code.append(area.stripLeading());
            } else {
                code.append(' ').append(area);
            }
        }
        return code.toString();
    }

    private static String expandTabs(String line) {
        if (line.indexOf('\t') < 0) {
            return line;
        }
        StringBuilder out = new StringBuilder();
        for (char c : line.toCharArray()) {
            if (c == '\t') {
                do {
                    out.append(' ');
                } while (out.length() % TAB_WIDTH != 0);
            } else {
                out.append(c);
            }
        }
        return out.toString();
    }

    /** Splits on separator periods (a period followed by a space or the end), outside literals. */
    private static List<List<String>> statements(String code) {
        List<List<String>> statements = new ArrayList<>();
        List<String> tokens = new ArrayList<>();
        StringBuilder token = new StringBuilder();
        char quote = 0;
        for (int i = 0; i < code.length(); i++) {
            char c = code.charAt(i);
            if (quote != 0) {
                token.append(c);
                if (c == quote) {
                    quote = 0;
                }
            } else if (c == '\'' || c == '"') {
                quote = c;
                token.append(c);
            } else if (c == '.' && (i + 1 == code.length() || Character.isWhitespace(code.charAt(i + 1)))) {
                flush(token, tokens);
                if (!tokens.isEmpty()) {
                    statements.add(tokens);
                }
                tokens = new ArrayList<>();
            } else if (Character.isWhitespace(c) || c == ',' || c == ';') {
                flush(token, tokens);
            } else {
                token.append(c);
            }
        }
        flush(token, tokens);
        if (!tokens.isEmpty()) {
            statements.add(tokens);
        }
        return statements;
    }

    private static void flush(StringBuilder token, List<String> tokens) {
        if (!token.isEmpty()) {
            tokens.add(token.toString());
            token.setLength(0);
        }
    }

    private List<Entry> parseEntries(List<List<String>> statements) {
        List<Entry> entries = new ArrayList<>();
        for (List<String> tokens : statements) {
            int level;
            try {
                level = Integer.parseInt(tokens.get(0));
            } catch (NumberFormatException e) {
                throw error("expected a level number, found '" + String.join(" ", tokens) + "'");
            }
            if (level == 88 || level == 66) {
                continue;
            }
            if (level < 1 || (level > 49 && level != 77)) {
                throw error("unsupported level " + level);
            }
            entries.add(parseEntry(level, tokens));
        }
        return entries;
    }

    private Entry parseEntry(int level, List<String> tokens) {
        Entry entry = new Entry(level);
        int i = 1;
        if (i < tokens.size() && !isKeyword(tokens.get(i))) {
            entry.name = tokens.get(i).toUpperCase(Locale.ROOT);
            i++;
        }
        while (i < tokens.size()) {
            String word = tokens.get(i).toUpperCase(Locale.ROOT);
            switch (word) {
                case "PIC", "PICTURE" -> {
                    i = skip(tokens, i + 1, "IS");
                    entry.picture = Picture.parse(token(tokens, i, word));
                    i++;
                }
                case "USAGE" -> i = skip(tokens, i + 1, "IS");
                case "COMP", "COMP-4", "COMP-5", "COMPUTATIONAL", "COMPUTATIONAL-4", "COMPUTATIONAL-5", "BINARY" -> {
                    entry.usage = Usage.BINARY;
                    i++;
                }
                case "COMP-3", "COMPUTATIONAL-3", "PACKED-DECIMAL" -> {
                    entry.usage = Usage.PACKED_DECIMAL;
                    i++;
                }
                case "DISPLAY" -> {
                    entry.usage = Usage.DISPLAY;
                    i++;
                }
                case "OCCURS" -> {
                    String count = token(tokens, i + 1, word);
                    try {
                        entry.occurs = Integer.parseInt(count);
                    } catch (NumberFormatException e) {
                        throw error("unsupported OCCURS " + count + " on " + entry.name);
                    }
                    i += 2;
                    if (i < tokens.size() && tokens.get(i).equalsIgnoreCase("TO")) {
                        throw error("OCCURS DEPENDING ON is not supported (" + entry.name + ")");
                    }
                    i = skip(tokens, i, "TIMES");
                }
                case "REDEFINES" -> {
                    entry.redefines = token(tokens, i + 1, word).toUpperCase(Locale.ROOT);
                    i += 2;
                }
                case "VALUE", "VALUES" -> {
                    i++;
                    while (i < tokens.size() && !isKeyword(tokens.get(i))) {
                        i++;
                    }
                }
                case "SYNC", "SYNCHRONIZED", "JUST", "JUSTIFIED", "EXTERNAL", "GLOBAL", "IS", "RIGHT" -> i++;
                case "SIGN", "BLANK" -> throw error(word + " clause is not supported (" + entry.name + ")");
                default -> throw error("unexpected '" + tokens.get(i) + "' in the entry for " + entry.name);
            }
        }
        if (entry.picture != null && entry.picture.category() != Picture.Category.NUMERIC
                && entry.usage != null && entry.usage != Usage.DISPLAY) {
            throw error(entry.name + ": USAGE " + entry.usage + " needs a numeric PIC");
        }
        return entry;
    }

    private static boolean isKeyword(String token) {
        return CLAUSE_KEYWORDS.contains(token.toUpperCase(Locale.ROOT));
    }

    private static int skip(List<String> tokens, int i, String optional) {
        return i < tokens.size() && tokens.get(i).equalsIgnoreCase(optional) ? i + 1 : i;
    }

    private String token(List<String> tokens, int i, String after) {
        if (i >= tokens.size()) {
            throw error("missing operand after " + after);
        }
        return tokens.get(i);
    }

    private List<Entry> buildTree(List<Entry> entries) {
        List<Entry> roots = new ArrayList<>();
        Deque<Entry> stack = new ArrayDeque<>();
        for (Entry entry : entries) {
            int level = entry.level == 77 ? 1 : entry.level;
            while (!stack.isEmpty() && stack.peek().effectiveLevel() >= level) {
                stack.pop();
            }
            if (stack.isEmpty()) {
                roots.add(entry);
            } else {
                Entry parent = stack.peek();
                if (parent.picture != null) {
                    throw error(parent.name + " has a PIC but also subordinate items");
                }
                parent.children.add(entry);
            }
            stack.push(entry);
        }
        return roots;
    }

    private CopybookItem resolve(Entry entry, int offset, Usage inherited) {
        Usage usage = entry.usage != null ? entry.usage : inherited;
        if (entry.picture == null && entry.children.isEmpty()) {
            throw error(entry.name + " has neither a PIC nor subordinate items");
        }
        if (entry.picture != null) {
            int length = entry.picture.byteLength(usage);
            return new CopybookItem(entry.level, entry.name, entry.picture, usage, entry.occurs, entry.redefines,
                    offset, length, List.of());
        }
        List<CopybookItem> children = new ArrayList<>();
        Map<String, Integer> offsets = new HashMap<>();
        int cursor = offset;
        int end = offset;
        for (Entry child : entry.children) {
            int childOffset = cursor;
            if (child.redefines != null) {
                Integer target = offsets.get(child.redefines);
                if (target == null) {
                    throw error(child.name + " redefines " + child.redefines + ", which is not an earlier sibling");
                }
                childOffset = target;
            }
            CopybookItem item = resolve(child, childOffset, usage);
            children.add(item);
            offsets.put(item.name(), childOffset);
            int childEnd = childOffset + item.totalLength();
            if (child.redefines == null) {
                cursor = childEnd;
            }
            end = Math.max(end, childEnd);
        }
        return new CopybookItem(entry.level, entry.name, null, usage, entry.occurs, entry.redefines, offset,
                end - offset, children);
    }

    private CopybookParseException error(String message) {
        return new CopybookParseException(copybook + ": " + message);
    }

    private static final class Entry {
        final int level;
        String name = "FILLER";
        Picture picture;
        Usage usage;
        int occurs = 1;
        String redefines;
        final List<Entry> children = new ArrayList<>();

        Entry(int level) {
            this.level = level;
        }

        int effectiveLevel() {
            return level == 77 ? 1 : level;
        }
    }
}
