package com.carddemo.legacyio;

import com.carddemo.legacyio.copybook.Copybook;
import com.carddemo.legacyio.copybook.CopybookItem;
import com.carddemo.legacyio.copybook.Picture;
import com.carddemo.legacyio.copybook.Usage;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

/**
 * The flat list of elementary fields of one record, as a program sees it: each {@code REDEFINES} alternative is either
 * the original item or one chosen redefinition, {@code OCCURS} items are expanded, and the fields tile the record
 * from byte 0 to {@link #length()} without gaps or overlaps.
 */
public final class RecordLayout {

    private final String name;
    private final int length;
    private final List<FieldDefinition> fields;
    private final Map<String, FieldDefinition> byName;
    private final Map<String, Span> spans;

    /** Byte range of a field or group. */
    public record Span(int offset, int length) {
    }

    private RecordLayout(String name, int length, List<FieldDefinition> fields, Map<String, Span> spans) {
        this.name = name;
        this.length = length;
        this.fields = List.copyOf(fields);
        Map<String, FieldDefinition> index = new LinkedHashMap<>();
        for (FieldDefinition field : fields) {
            if (index.putIfAbsent(field.name(), field) != null) {
                throw new IllegalArgumentException(name + " has two fields named " + field.name());
            }
        }
        this.byName = Collections.unmodifiableMap(index);
        this.spans = Collections.unmodifiableMap(spans);
    }

    /** Layout of a record with no redefinitions chosen (every {@code REDEFINES} item is skipped). */
    public static RecordLayout of(CopybookItem record) {
        return of(record, Set.of());
    }

    /** Layout of the copybook's only record. */
    public static RecordLayout of(Copybook copybook, String... redefinitions) {
        return of(copybook.record(), Set.of(redefinitions));
    }

    /**
     * Layout of a record using the named {@code REDEFINES} items in place of the items they redefine.
     *
     * @param redefinitions names of {@code REDEFINES} items to select
     */
    public static RecordLayout of(CopybookItem record, Set<String> redefinitions) {
        Set<String> replaced = new HashSet<>();
        Set<String> unknown = new HashSet<>(redefinitions);
        collectReplaced(record, redefinitions, replaced, unknown);
        if (!unknown.isEmpty()) {
            throw new IllegalArgumentException(record.name() + " has no REDEFINES items named " + unknown);
        }
        List<FieldDefinition> fields = new ArrayList<>();
        Map<String, Span> spans = new LinkedHashMap<>();
        flatten(record, 0, "", redefinitions, replaced, fields, spans);
        fields.sort((a, b) -> Integer.compare(a.offset(), b.offset()));
        int start = record.offset();
        List<FieldDefinition> tiled = new ArrayList<>();
        int cursor = 0;
        for (FieldDefinition field : fields) {
            FieldDefinition relative = new FieldDefinition(field.name(), field.offset() - start, field.length(),
                    field.picture(), field.usage());
            if (relative.offset() < cursor) {
                throw new IllegalArgumentException(record.name() + ": field " + relative.name()
                        + " overlaps the field before it; select one REDEFINES alternative per area");
            }
            if (relative.offset() > cursor) {
                tiled.add(gapFiller(cursor, relative.offset() - cursor));
            }
            tiled.add(relative);
            cursor = relative.end();
        }
        if (cursor < record.totalLength()) {
            tiled.add(gapFiller(cursor, record.totalLength() - cursor));
        }
        Map<String, Span> relativeSpans = new LinkedHashMap<>();
        spans.forEach((k, v) -> relativeSpans.put(k, new Span(v.offset() - start, v.length())));
        return new RecordLayout(record.name(), record.totalLength(), tiled, relativeSpans);
    }

    private static FieldDefinition gapFiller(int offset, int length) {
        return new FieldDefinition("FILLER@" + offset, offset, length,
                new Picture("X(" + length + ")", Picture.Category.ALPHANUMERIC, length, 0, false), Usage.DISPLAY);
    }

    private static void collectReplaced(CopybookItem item, Set<String> chosen, Set<String> replaced, Set<String> unknown) {
        for (CopybookItem child : item.children()) {
            if (child.redefines() != null && chosen.contains(child.name())) {
                unknown.remove(child.name());
                if (!replaced.add(child.redefines())) {
                    throw new IllegalArgumentException("More than one redefinition of " + child.redefines() + " selected");
                }
            }
            collectReplaced(child, chosen, replaced, unknown);
        }
    }

    private static void flatten(CopybookItem item, int shift, String subscript, Set<String> chosen,
            Set<String> replaced, List<FieldDefinition> out, Map<String, Span> spans) {
        if (item.redefines() != null && !chosen.contains(item.name()) && item.level() != 1) {
            return;
        }
        if (replaced.contains(item.name())) {
            return;
        }
        for (int occurrence = 1; occurrence <= item.occurs(); occurrence++) {
            int itemShift = shift + (occurrence - 1) * item.length();
            String itemSubscript = item.occurs() > 1
                    ? (subscript.isEmpty() ? String.valueOf(occurrence) : subscript + "," + occurrence)
                    : subscript;
            int offset = item.offset() + itemShift;
            String name = itemSubscript.isEmpty() ? item.name() : item.name() + "(" + itemSubscript + ")";
            if (item.isGroup()) {
                if (!item.isFiller()) {
                    spans.putIfAbsent(name, new Span(offset, item.length()));
                }
                for (CopybookItem child : item.children()) {
                    flatten(child, itemShift, itemSubscript, chosen, replaced, out, spans);
                }
            } else {
                String fieldName = item.isFiller() ? "FILLER@" + offset : name;
                out.add(new FieldDefinition(fieldName, offset, item.length(), item.picture(), item.usage()));
                if (!item.isFiller()) {
                    spans.putIfAbsent(fieldName, new Span(offset, item.length()));
                }
            }
        }
    }

    public String name() {
        return name;
    }

    /** Record length in bytes. */
    public int length() {
        return length;
    }

    /** Fields in record order. */
    public List<FieldDefinition> fields() {
        return fields;
    }

    public FieldDefinition field(String fieldName) {
        FieldDefinition field = byName.get(fieldName);
        if (field == null) {
            throw new NoSuchElementException(name + " has no field " + fieldName);
        }
        return field;
    }

    public boolean hasField(String fieldName) {
        return byName.containsKey(fieldName);
    }

    /** Byte range of a named field or group, e.g. a VSAM key group such as {@code TRAN-CAT-KEY}. */
    public Span span(String itemName) {
        Span span = spans.get(itemName);
        if (span == null) {
            throw new NoSuchElementException(name + " has no item " + itemName);
        }
        return span;
    }

    @Override
    public String toString() {
        return "RecordLayout[" + name + ", " + length + " bytes, " + fields.size() + " fields]";
    }
}
