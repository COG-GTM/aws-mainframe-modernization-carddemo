package com.carddemo.common.copybook;

import com.carddemo.common.cobol.CobolDataException;
import com.carddemo.common.cobol.PicClause;
import com.carddemo.common.cobol.SignMode;
import com.carddemo.common.cobol.Usage;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;
import java.util.Locale;
import java.util.Optional;
import java.util.Set;

/**
 * Parses the DATA DIVISION entries of a fixed-format COBOL copybook into {@link RecordLayout}s.
 *
 * <p>Handles sequence/identification areas (columns 1-6 and 73-80), comment lines, levels 01-49/77/88
 * (66 RENAMES is skipped), PIC, USAGE (DISPLAY, COMP/BINARY, COMP-3, COMP-5, inherited from groups), SIGN,
 * OCCURS [DEPENDING ON] (sized at its maximum), REDEFINES (alternate views share storage), FILLER and VALUE.
 * Copybooks without an 01 level (meant to be COPYed under one) get an implicit 01 named after the copybook.
 * Copybooks holding PROCEDURE DIVISION code (e.g. CSUTLDPY, CSSTRPFY) yield no records.
 */
public final class CopybookParser {

    private static final Set<String> KEYWORDS = Set.of("PIC", "PICTURE", "USAGE", "OCCURS", "REDEFINES", "VALUE",
            "VALUES", "SIGN", "LEADING", "TRAILING", "SYNC", "SYNCHRONIZED", "JUST", "JUSTIFIED", "BLANK", "GLOBAL",
            "EXTERNAL", "DISPLAY", "COMP", "COMP-3", "COMP-4", "COMP-5", "COMPUTATIONAL", "COMPUTATIONAL-3",
            "COMPUTATIONAL-4", "COMPUTATIONAL-5", "BINARY", "PACKED-DECIMAL", "INDEXED", "ASCENDING", "DESCENDING",
            "DEPENDING", "TIMES", "TO", "IS", "ARE", "ON", "BY", "KEY");

    private CopybookParser() {
    }

    public static Copybook parse(Path path) {
        try {
            String fileName = path.getFileName().toString();
            String name = fileName.contains(".") ? fileName.substring(0, fileName.lastIndexOf('.')) : fileName;
            return parse(name.toUpperCase(Locale.ROOT), Files.readString(path, StandardCharsets.ISO_8859_1));
        } catch (IOException e) {
            throw new CobolDataException("Cannot read copybook " + path, e);
        }
    }

    public static Copybook parse(String name, String source) {
        List<List<String>> entries = entries(normalize(source));
        List<RecordLayout> records = new ArrayList<>();
        Deque<CopybookField> stack = new ArrayDeque<>();
        CopybookField last = null;
        for (List<String> entry : entries) {
            Optional<Integer> level = level(entry.get(0));
            if (level.isEmpty()) {
                continue; // procedure code, COPY, EJECT, ...
            }
            int lvl = level.get();
            if (lvl == 66) {
                continue;
            }
            if (lvl == 88) {
                if (last == null) {
                    throw new CobolDataException("Level 88 without a parent item in " + name + ": " + entry);
                }
                last.addCondition(condition(entry));
                continue;
            }
            if (lvl == 1 || lvl == 77) {
                stack.clear();
            }
            while (!stack.isEmpty() && stack.peek().level() >= lvl) {
                stack.pop();
            }
            if (stack.isEmpty() && lvl != 1 && lvl != 77) {
                // Copybook meant to be COPYed under a program's 01 level (e.g. CIPAUSMY): synthesize that root.
                CopybookField implicitRoot = new CopybookField(1, name, null, Usage.DISPLAY,
                        SignMode.TRAILING_OVERPUNCH, 1, null, null);
                records.add(new RecordLayout(implicitRoot));
                stack.push(implicitRoot);
            }
            CopybookField parent = stack.peek();
            CopybookField field = item(lvl, entry, parent, name);
            if (parent == null) {
                records.add(new RecordLayout(field));
            } else {
                parent.addChild(field);
            }
            stack.push(field);
            last = field;
        }
        for (RecordLayout record : records) {
            layout(record.root(), 0);
        }
        return new Copybook(name, List.copyOf(records));
    }

    private static CopybookField item(int level, List<String> t, CopybookField parent, String copybook) {
        int i = 1;
        String name = "FILLER";
        if (t.size() > 1 && !KEYWORDS.contains(t.get(1).toUpperCase(Locale.ROOT))) {
            name = t.get(1).toUpperCase(Locale.ROOT);
            i = 2;
        }
        PicClause pic = null;
        Usage usage = null;
        SignMode sign = null;
        int occurs = 1;
        String dependingOn = null;
        String redefines = null;
        while (i < t.size()) {
            String kw = t.get(i).toUpperCase(Locale.ROOT);
            switch (kw) {
                case "PIC", "PICTURE" -> {
                    i = skip(t, i + 1, "IS");
                    pic = PicClause.parse(t.get(i++));
                }
                case "USAGE" -> {
                    i = skip(t, i + 1, "IS");
                    usage = usage(t.get(i++), copybook, name);
                }
                case "OCCURS" -> {
                    occurs = Integer.parseInt(t.get(++i));
                    i++;
                    if (i < t.size() && t.get(i).equalsIgnoreCase("TO")) {
                        occurs = Integer.parseInt(t.get(i + 1));
                        i += 2;
                    }
                    i = skip(t, i, "TIMES");
                    while (i < t.size()) {
                        String sub = t.get(i).toUpperCase(Locale.ROOT);
                        if (sub.equals("DEPENDING")) {
                            i = skip(t, i + 1, "ON");
                            dependingOn = t.get(i++).toUpperCase(Locale.ROOT);
                        } else if (sub.equals("ASCENDING") || sub.equals("DESCENDING") || sub.equals("INDEXED")) {
                            i = skip(t, i + 1, "KEY", "IS", "BY");
                            while (i < t.size() && !KEYWORDS.contains(t.get(i).toUpperCase(Locale.ROOT))) {
                                i++;
                            }
                        } else {
                            break;
                        }
                    }
                }
                case "REDEFINES" -> {
                    redefines = t.get(i + 1).toUpperCase(Locale.ROOT);
                    i += 2;
                }
                case "VALUE", "VALUES" -> {
                    i = skip(t, i + 1, "IS", "ARE");
                    if (i < t.size() && t.get(i).equalsIgnoreCase("ALL")) {
                        i++;
                    }
                    i++;
                }
                case "SIGN", "LEADING", "TRAILING" -> {
                    if (kw.equals("SIGN")) {
                        i = skip(t, i + 1, "IS");
                        kw = t.get(i).toUpperCase(Locale.ROOT);
                    }
                    boolean leading = kw.equals("LEADING");
                    i++;
                    boolean separate = i < t.size() && t.get(i).equalsIgnoreCase("SEPARATE");
                    if (separate) {
                        i = skip(t, i + 1, "CHARACTER");
                    }
                    sign = leading ? (separate ? SignMode.LEADING_SEPARATE : SignMode.LEADING_OVERPUNCH)
                            : (separate ? SignMode.TRAILING_SEPARATE : SignMode.TRAILING_OVERPUNCH);
                }
                case "SYNC", "SYNCHRONIZED", "JUST", "JUSTIFIED" -> i = skip(t, i + 1, "LEFT", "RIGHT");
                case "BLANK" -> i = skip(t, i + 1, "WHEN") + 1;
                case "GLOBAL", "EXTERNAL" -> i++;
                default -> {
                    Optional<Usage> u = Usage.fromKeyword(kw);
                    if (u.isEmpty()) {
                        throw new CobolDataException("Unexpected token '" + t.get(i) + "' in " + copybook + " entry " + t);
                    }
                    usage = u.get();
                    i++;
                }
            }
        }
        Usage effectiveUsage = usage != null ? usage : parent != null ? parent.usage() : Usage.DISPLAY;
        SignMode effectiveSign = sign != null ? sign : parent != null ? parent.signMode() : SignMode.TRAILING_OVERPUNCH;
        return new CopybookField(level, name, pic, effectiveUsage, effectiveSign, occurs, dependingOn, redefines);
    }

    private static Usage usage(String token, String copybook, String name) {
        return Usage.fromKeyword(token).orElseThrow(() ->
                new CobolDataException("Unknown USAGE " + token + " for " + name + " in " + copybook));
    }

    private static ConditionName condition(List<String> t) {
        String name = t.get(1).toUpperCase(Locale.ROOT);
        int i = skip(t, 2, "VALUE", "VALUES", "IS");
        i = skip(t, i, "ARE");
        List<String> values = new ArrayList<>();
        while (i < t.size()) {
            String v = t.get(i++);
            if ((v.equalsIgnoreCase("THRU") || v.equalsIgnoreCase("THROUGH")) && !values.isEmpty() && i < t.size()) {
                values.set(values.size() - 1, values.get(values.size() - 1) + " THRU " + t.get(i++));
            } else {
                values.add(v);
            }
        }
        return new ConditionName(name, List.copyOf(values));
    }

    private static void layout(CopybookField field, int start) {
        if (!field.isGroup()) {
            field.position(start, field.usage().storageLength(field.pic(), field.signMode()));
            return;
        }
        int cursor = start;
        int end = start;
        List<CopybookField> children = field.children();
        for (int c = 0; c < children.size(); c++) {
            CopybookField child = children.get(c);
            int childStart = cursor;
            if (child.redefines() != null) {
                CopybookField target = null;
                for (int p = c - 1; p >= 0 && target == null; p--) {
                    if (children.get(p).name().equals(child.redefines())) {
                        target = children.get(p);
                    }
                }
                if (target == null) {
                    throw new CobolDataException(child.name() + " REDEFINES unknown sibling " + child.redefines());
                }
                childStart = target.offset();
            }
            layout(child, childStart);
            cursor = Math.max(cursor, childStart + child.totalLength());
            end = Math.max(end, childStart + child.totalLength());
        }
        field.position(start, end - start);
    }

    /** Joins the program text area (columns 8-72) of non-comment lines. */
    static String normalize(String source) {
        StringBuilder sb = new StringBuilder();
        for (String raw : source.split("\r?\n")) {
            String line = raw.replace('\t', ' ');
            if (line.isBlank() || (line.length() < 7 && line.matches("[0-9 ]*"))) {
                continue;
            }
            boolean fixedFormat = line.length() >= 7 && line.substring(0, 6).matches("[0-9 ]{6}");
            if (!fixedFormat) {
                if (line.stripLeading().startsWith("*")) {
                    continue;
                }
                sb.append(' ').append(line);
                continue;
            }
            char indicator = line.charAt(6);
            if (indicator == '*' || indicator == '/') {
                continue;
            }
            String area = line.length() > 7 ? line.substring(7, Math.min(72, line.length())) : "";
            if (indicator == '-') {
                String cont = area.stripLeading();
                if (cont.startsWith("'") || cont.startsWith("\"")) {
                    int open = sb.lastIndexOf(cont.substring(0, 1));
                    String tail = sb.substring(open + 1);
                    sb.setLength(open + 1);
                    sb.append(tail).append(cont.substring(1));
                } else {
                    sb.append(cont);
                }
                continue;
            }
            sb.append(' ').append(area);
        }
        return sb.toString();
    }

    /** Splits normalized text into period-terminated entries of tokens (quoted literals kept whole). */
    static List<List<String>> entries(String text) {
        List<List<String>> entries = new ArrayList<>();
        List<String> current = new ArrayList<>();
        int i = 0;
        int n = text.length();
        while (i < n) {
            char c = text.charAt(i);
            if (Character.isWhitespace(c) || c == ',' || c == ';') {
                i++;
                continue;
            }
            int start = i;
            boolean end;
            if (c == '\'' || c == '"') {
                i++;
                while (i < n) {
                    if (text.charAt(i) == c) {
                        if (i + 1 < n && text.charAt(i + 1) == c) {
                            i += 2;
                            continue;
                        }
                        break;
                    }
                    i++;
                }
                i = Math.min(i + 1, n);
                current.add(text.substring(start, i));
                end = i < n && text.charAt(i) == '.' && (i + 1 == n || Character.isWhitespace(text.charAt(i + 1)));
                if (end) {
                    i++;
                }
            } else {
                while (i < n && !Character.isWhitespace(text.charAt(i))) {
                    i++;
                }
                String token = text.substring(start, i);
                end = token.endsWith(".");
                if (end) {
                    token = token.substring(0, token.length() - 1);
                }
                if (token.endsWith(",")) {
                    token = token.substring(0, token.length() - 1);
                }
                if (!token.isEmpty()) {
                    current.add(token);
                }
            }
            if (end && !current.isEmpty()) {
                entries.add(current);
                current = new ArrayList<>();
            }
        }
        if (!current.isEmpty()) {
            entries.add(current);
        }
        return entries;
    }

    private static Optional<Integer> level(String token) {
        if (!token.matches("\\d{1,2}")) {
            return Optional.empty();
        }
        int level = Integer.parseInt(token);
        return (level >= 1 && level <= 49) || level == 66 || level == 77 || level == 88
                ? Optional.of(level) : Optional.empty();
    }

    /** Skips each optional noise word, in order, when present at the cursor. */
    private static int skip(List<String> t, int i, String... optionalWords) {
        for (String word : optionalWords) {
            if (i < t.size() && t.get(i).equalsIgnoreCase(word)) {
                i++;
            }
        }
        return i;
    }
}
