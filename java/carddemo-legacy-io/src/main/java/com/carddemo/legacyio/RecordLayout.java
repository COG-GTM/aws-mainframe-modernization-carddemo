package com.carddemo.legacyio;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

/**
 * The byte layout of one record: its elementary fields in order, covering every byte exactly once.
 *
 * <p>A REDEFINES item shares the bytes of the item it redefines. The default layout uses the redefined (original)
 * items; {@link #view(String...)} swaps in redefinitions by name, e.g. {@code view("EXPORT-ACCOUNT-DATA")} for the
 * account variant of {@code CVEXPORT}.
 */
public final class RecordLayout {

    private final CopybookItem root;
    private final Set<String> redefinitions;
    private final String name;
    private final int length;
    private final List<FieldDefinition> fields;
    private final Map<String, Integer> fieldIndex = new HashMap<>();
    private final Map<String, ItemSpan> spans = new LinkedHashMap<>();
    private final Set<String> ambiguous = new HashSet<>();

    private RecordLayout(CopybookItem root, Set<String> redefinitions) {
        this.root = root;
        this.redefinitions = Set.copyOf(redefinitions);
        this.name = root.name();
        List<FieldDefinition> placed = new ArrayList<>();
        Set<String> used = new HashSet<>();
        this.length = place(root, 0, "", placed, used);
        Set<String> unknown = new HashSet<>(this.redefinitions);
        unknown.removeAll(used);
        if (!unknown.isEmpty()) {
            throw new IllegalArgumentException("record " + name + " has no REDEFINES item named " + unknown);
        }
        this.fields = List.copyOf(placed);
        for (int i = 0; i < fields.size(); i++) {
            String fieldName = fields.get(i).name();
            if (fieldIndex.putIfAbsent(fieldName, i) != null) {
                ambiguous.add(fieldName);
            }
        }
    }

    static RecordLayout of(CopybookItem root) {
        return new RecordLayout(root, Set.of());
    }

    /** This record with the named REDEFINES items in place of the items they redefine. */
    public RecordLayout view(String... redefiningItems) {
        Set<String> names = new HashSet<>(redefinitions);
        for (String item : redefiningItems) {
            names.add(item.toUpperCase(Locale.ROOT));
        }
        return new RecordLayout(root, names);
    }

    public String name() {
        return name;
    }

    public int length() {
        return length;
    }

    public List<FieldDefinition> fields() {
        return fields;
    }

    public FieldDefinition field(String fieldName) {
        return fields.get(indexOf(fieldName));
    }

    public boolean hasField(String fieldName) {
        return fieldIndex.containsKey(fieldName.toUpperCase(Locale.ROOT));
    }

    /** Byte range of a group or elementary item in this layout. */
    public ItemSpan span(String itemName) {
        String key = itemName.toUpperCase(Locale.ROOT);
        if (ambiguous.contains(key)) {
            throw new IllegalArgumentException("item " + itemName + " is not unique in " + name);
        }
        ItemSpan span = spans.get(key);
        if (span == null) {
            throw new IllegalArgumentException("record " + name + " has no item " + itemName);
        }
        return span;
    }

    int indexOf(String fieldName) {
        String key = fieldName.toUpperCase(Locale.ROOT);
        if (ambiguous.contains(key)) {
            throw new IllegalArgumentException("field " + fieldName + " is not unique in " + name);
        }
        Integer index = fieldIndex.get(key);
        if (index == null) {
            throw new IllegalArgumentException("record " + name + " has no field " + fieldName);
        }
        return index;
    }

    private int place(CopybookItem item, int offset, String subscripts, List<FieldDefinition> out, Set<String> used) {
        int occurrence = item.occurrenceLength();
        for (int n = 1; n <= item.occurs(); n++) {
            String subs = item.occurs() == 1 ? subscripts
                    : subscripts.isEmpty() ? String.valueOf(n) : subscripts + "," + n;
            int start = offset + (n - 1) * occurrence;
            String itemName = subs.isEmpty() ? item.name() : item.name() + "(" + subs + ")";
            if (!item.name().equals("FILLER") && spans.putIfAbsent(itemName, new ItemSpan(itemName, start, occurrence)) != null) {
                ambiguous.add(itemName);
            }
            if (!item.isGroup()) {
                out.add(new FieldDefinition(itemName, start, occurrence, item.picture(), item.usage()));
                continue;
            }
            int cursor = start;
            for (CopybookItem child : item.children()) {
                if (child.redefines() != null) {
                    continue;
                }
                CopybookItem chosen = chosenFor(item, child, used);
                int written = place(chosen, cursor, subs, out, used);
                int gap = child.totalLength() - written;
                if (gap > 0) {
                    out.add(new FieldDefinition("FILLER", cursor + written, gap,
                            new Picture(Picture.Category.ALPHANUMERIC, gap, 0, 0, false), Usage.DISPLAY));
                }
                cursor += child.totalLength();
            }
        }
        return occurrence * item.occurs();
    }

    private CopybookItem chosenFor(CopybookItem parent, CopybookItem original, Set<String> used) {
        CopybookItem chosen = original;
        for (CopybookItem sibling : parent.children()) {
            if (original.name().equals(sibling.redefines()) && redefinitions.contains(sibling.name())) {
                if (chosen != original) {
                    throw new IllegalArgumentException(
                            "both " + chosen.name() + " and " + sibling.name() + " redefine " + original.name());
                }
                if (sibling.totalLength() > original.totalLength()) {
                    throw new IllegalArgumentException(sibling.name() + " is longer than " + original.name());
                }
                used.add(sibling.name());
                chosen = sibling;
            }
        }
        return chosen;
    }

    @Override
    public String toString() {
        return name + "(" + length + " bytes" + (redefinitions.isEmpty() ? "" : ", view " + redefinitions) + ")";
    }
}
