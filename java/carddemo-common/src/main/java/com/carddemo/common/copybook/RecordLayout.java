package com.carddemo.common.copybook;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Optional;

/** Resolved storage layout of one 01-level record. */
public final class RecordLayout {

    private final CopybookField root;

    RecordLayout(CopybookField root) {
        this.root = root;
    }

    public String name() {
        return root.name();
    }

    public CopybookField root() {
        return root;
    }

    /** Record length in bytes (LRECL for fixed-length files). */
    public int length() {
        return root.totalLength();
    }

    /** All items depth-first, groups included, REDEFINES alternates included. */
    public List<CopybookField> allFields() {
        List<CopybookField> out = new ArrayList<>();
        collect(root, out);
        return out;
    }

    /** Elementary items (excluding FILLER) of the primary storage view, i.e. ignoring REDEFINES alternates. */
    public List<CopybookField> primaryElementaryFields() {
        return allFields().stream()
                .filter(f -> !f.isGroup() && !f.isFiller() && !f.isAlternateView())
                .toList();
    }

    /** Finds an item by its (unique) data name. Use {@link #field(String, String)} to qualify ambiguous names. */
    public CopybookField field(String name) {
        return findField(name).orElseThrow(() -> new IllegalArgumentException(
                "No field " + name + " in record " + name()));
    }

    public Optional<CopybookField> findField(String name) {
        return find(root, name.toUpperCase(Locale.ROOT));
    }

    /** {@code name OF group} qualification. */
    public CopybookField field(String name, String group) {
        CopybookField g = field(group);
        return find(g, name.toUpperCase(Locale.ROOT)).orElseThrow(() -> new IllegalArgumentException(
                "No field " + name + " OF " + group + " in record " + name()));
    }

    static Optional<CopybookField> find(CopybookField scope, String name) {
        List<CopybookField> matches = new ArrayList<>();
        collect(scope, matches);
        List<CopybookField> named = matches.stream().filter(f -> f.name().equals(name)).toList();
        if (named.size() > 1) {
            throw new IllegalArgumentException("Ambiguous field name " + name + " in " + scope.name()
                    + "; qualify it with field(name, group)");
        }
        return named.stream().findFirst();
    }

    private static void collect(CopybookField f, List<CopybookField> out) {
        out.add(f);
        for (CopybookField c : f.children()) {
            collect(c, out);
        }
    }

    /** Human-readable layout listing (one line per item) for diagnostics and documentation. */
    public String describe() {
        StringBuilder sb = new StringBuilder();
        for (CopybookField f : allFields()) {
            int depth = 0;
            for (CopybookField p = f.parent(); p != null; p = p.parent()) {
                depth++;
            }
            sb.append("  ".repeat(depth)).append(f).append('\n');
        }
        return sb.toString();
    }

    @Override
    public String toString() {
        return "RecordLayout[" + name() + ", length=" + length() + "]";
    }
}
