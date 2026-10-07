package com.carddemo.common.codec;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

/**
 * The layout of one {@code 01} record of a copybook.
 *
 * <p>{@link #leaves(String...)} flattens the record into its elementary items in storage order, expanding
 * {@code OCCURS} tables into subscripted keys ({@code EXP-CUST-ADDR-LINE(2)}). Where items {@code REDEFINES}
 * one another only the original is visited unless the redefining item's name is passed as active, in which
 * case it replaces the original; this is how a caller selects the record variant (e.g. by record type).
 */
public final class RecordLayout {

    /** An elementary item reached by {@link #leaves(String...)} with its decode key. */
    public record Leaf(String key, Field field) {
    }

    private final Field root;
    private final List<Field> fields;

    RecordLayout(Field root) {
        this.root = root;
        List<Field> all = new ArrayList<>();
        collect(root, all);
        this.fields = List.copyOf(all);
    }

    public String name() {
        return root.name();
    }

    public int length() {
        return root.totalSize();
    }

    public Field root() {
        return root;
    }

    /** Every item, the record itself first, in source order. */
    public List<Field> fields() {
        return fields;
    }

    /**
     * Looks an item up by name, optionally qualified as COBOL {@code name OF q1 OF q2} (nearest group first).
     *
     * @throws IllegalArgumentException when the name is unknown or ambiguous
     */
    public Field field(String name, String... qualifiers) {
        List<Field> matches = fields.stream()
                .filter(f -> f.name().equalsIgnoreCase(name) && qualifiedBy(f, qualifiers))
                .toList();
        if (matches.size() != 1) {
            throw new IllegalArgumentException(name() + ": " + (matches.isEmpty() ? "no" : "ambiguous")
                    + " item " + name + (qualifiers.length == 0 ? "" : " OF " + String.join(" OF ", qualifiers)));
        }
        return matches.get(0);
    }

    public List<Leaf> leaves(String... activeRedefines) {
        Set<String> active = new TreeSet<>(String.CASE_INSENSITIVE_ORDER);
        active.addAll(Arrays.asList(activeRedefines));
        List<Field> unsubscripted = new ArrayList<>();
        List<int[]> subscripts = new ArrayList<>();
        visit(root, new int[0], active, unsubscripted, subscripts);
        Map<String, Set<Field>> byName = new HashMap<>();
        for (Field f : unsubscripted) {
            byName.computeIfAbsent(f.name().toUpperCase(Locale.ROOT),
                    k -> Collections.newSetFromMap(new IdentityHashMap<>())).add(f);
        }
        List<Leaf> leaves = new ArrayList<>(unsubscripted.size());
        for (int i = 0; i < unsubscripted.size(); i++) {
            Field f = unsubscripted.get(i);
            int[] subs = subscripts.get(i);
            String key = f.name();
            if (subs.length > 0) {
                key += "(" + String.join(",", Arrays.stream(subs).mapToObj(Integer::toString).toList()) + ")";
            }
            if (byName.get(f.name().toUpperCase(Locale.ROOT)).size() > 1 && !f.ancestors().isEmpty()) {
                key += " OF " + f.ancestors().get(0);
            }
            leaves.add(new Leaf(key, f.subscript(subs)));
        }
        return leaves;
    }

    /** Decodes every non-FILLER elementary item: String, BigDecimal, or null for a numeric item of LOW-VALUES. */
    public Map<String, Object> decode(FixedWidthRecord record, String... activeRedefines) {
        requireLength(record);
        Map<String, Object> values = new LinkedHashMap<>();
        for (Leaf leaf : leaves(activeRedefines)) {
            if (!leaf.field().isFiller()) {
                values.put(leaf.key(), record.get(leaf.field()));
            }
        }
        return values;
    }

    /** Stores {@code values} (keyed as by {@link #decode}) into {@code record}; other bytes are left unchanged. */
    public void encode(Map<String, ?> values, FixedWidthRecord record, String... activeRedefines) {
        requireLength(record);
        for (Leaf leaf : leaves(activeRedefines)) {
            if (values.containsKey(leaf.key())) {
                record.set(leaf.field(), values.get(leaf.key()));
            }
        }
    }

    private void requireLength(FixedWidthRecord record) {
        if (record.length() != length()) {
            throw new RecordFormatException(name() + " is " + length() + " bytes, record is " + record.length());
        }
    }

    private static void visit(Field f, int[] subs, Set<String> active, List<Field> out, List<int[]> outSubs) {
        if (f.occurs() > 0) {
            for (int i = 1; i <= f.occurs(); i++) {
                int[] next = Arrays.copyOf(subs, subs.length + 1);
                next[subs.length] = i;
                visitInstance(f, next, active, out, outSubs);
            }
        } else {
            visitInstance(f, subs, active, out, outSubs);
        }
    }

    private static void visitInstance(Field f, int[] subs, Set<String> active, List<Field> out, List<int[]> outSubs) {
        if (!f.isGroup()) {
            out.add(f);
            outSubs.add(subs);
            return;
        }
        Set<String> replaced = new TreeSet<>(String.CASE_INSENSITIVE_ORDER);
        for (Field c : f.children()) {
            if (c.redefines() != null && active.contains(c.name())) {
                replaced.add(c.redefines());
            }
        }
        for (Field c : f.children()) {
            boolean inactiveRedefinition = c.redefines() != null && !active.contains(c.name());
            if (!inactiveRedefinition && !replaced.contains(c.name())) {
                visit(c, subs, active, out, outSubs);
            }
        }
    }

    private static boolean qualifiedBy(Field f, String[] qualifiers) {
        int next = 0;
        for (String ancestor : f.ancestors()) {
            if (next < qualifiers.length && ancestor.equalsIgnoreCase(qualifiers[next])) {
                next++;
            }
        }
        return next == qualifiers.length;
    }

    private static void collect(Field f, List<Field> out) {
        out.add(f);
        for (Field c : f.children()) {
            collect(c, out);
        }
    }

    @Override
    public String toString() {
        return "RecordLayout[" + name() + ", " + length() + " bytes]";
    }
}
