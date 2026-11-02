package com.carddemo.common.record;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * The byte layout of one copybook record: its primary storage map (the elementary items in order,
 * covering every byte once), plus any {@code REDEFINES} views and group overlays.
 */
public final class RecordLayout {

    private final String copybook;
    private final String recordName;
    private final int length;
    private final List<Field> fields;
    private final List<Field> allFields;
    private final Map<String, List<Field>> views;

    private RecordLayout(Builder root) {
        this.copybook = root.copybook;
        this.recordName = root.recordName;
        this.length = root.offset;
        this.fields = List.copyOf(root.primary);
        this.allFields = List.copyOf(root.all);
        Map<String, List<Field>> copy = new LinkedHashMap<>();
        root.views.forEach((name, list) -> copy.put(name, List.copyOf(list)));
        this.views = Collections.unmodifiableMap(copy);
    }

    public static Builder builder(String copybook, String recordName) {
        return new Builder(copybook, recordName);
    }

    /** Copybook member name, e.g. {@code CVACT01Y}. */
    public String copybook() {
        return copybook;
    }

    /** The 01-level record name, e.g. {@code ACCOUNT-RECORD}. */
    public String recordName() {
        return recordName;
    }

    /** Record length in bytes (the dataset's LRECL). */
    public int length() {
        return length;
    }

    /** Primary storage map: elementary items in order, covering bytes 0..length-1 exactly once. */
    public List<Field> fields() {
        return fields;
    }

    /** Every declared field, including REDEFINES views and group overlays. */
    public List<Field> allFields() {
        return allFields;
    }

    /** Fields of one {@code REDEFINES} view, e.g. {@code EXPORT-ACCOUNT-DATA}. */
    public List<Field> view(String name) {
        List<Field> view = views.get(name);
        if (view == null) {
            throw new IllegalArgumentException(copybook + " has no view " + name);
        }
        return view;
    }

    public Field field(String name) {
        return allFields.stream()
                .filter(f -> f.name().equals(name))
                .findFirst()
                .orElseThrow(() -> new IllegalArgumentException(copybook + " has no field " + name));
    }

    @Override
    public String toString() {
        return copybook + " " + recordName + " (" + length + " bytes)";
    }

    /**
     * Declares fields in copybook order. Use {@link #redefines} for a {@code REDEFINES} view and
     * {@link #group} for a group item that should be readable as one alphanumeric value (e.g. a key).
     */
    public static final class Builder {

        private final Builder root;
        private final String copybook;
        private final String recordName;
        private final String view;
        private final int limit;
        private int offset;

        private final List<Field> all;
        private final List<Field> primary;
        private final Map<String, List<Field>> views;
        private final List<Builder> redefines;
        private boolean built;

        private Builder(String copybook, String recordName) {
            this.root = this;
            this.copybook = copybook;
            this.recordName = recordName;
            this.view = null;
            this.limit = Integer.MAX_VALUE;
            this.offset = 0;
            this.all = new ArrayList<>();
            this.primary = new ArrayList<>();
            this.views = new LinkedHashMap<>();
            this.redefines = new ArrayList<>();
        }

        private Builder(Builder root, String view, int start, int limit) {
            this.root = root;
            this.copybook = root.copybook;
            this.recordName = root.recordName;
            this.view = view;
            this.limit = limit;
            this.offset = start;
            this.all = null;
            this.primary = null;
            this.views = null;
            this.redefines = null;
        }

        public AlphanumericField alpha(String name, String picture) {
            Picture pic = Picture.parse(picture);
            if (!pic.alphanumeric()) {
                throw new IllegalArgumentException(name + " is not alphanumeric: " + picture);
            }
            return add(new AlphanumericField(copybook, name, pic.text(), reserve(name, pic.length()), pic.length(), view));
        }

        public NumericField numeric(String name, String picture) {
            Picture pic = Picture.parse(picture);
            if (pic.alphanumeric()) {
                throw new IllegalArgumentException(name + " is not numeric: " + picture);
            }
            return add(new NumericField(copybook, name, pic.text(), reserve(name, pic.length()), pic.length(), view,
                    pic.digits(), pic.scale(), pic.signed(), pic.usage()));
        }

        /** {@code OCCURS n TIMES} of a {@code PIC X} item; names carry the subscript, e.g. {@code NAME(1)}. */
        public List<AlphanumericField> alphaOccurs(String name, String picture, int times) {
            List<AlphanumericField> items = new ArrayList<>(times);
            for (int i = 1; i <= times; i++) {
                items.add(alpha(name + "(" + i + ")", picture));
            }
            return List.copyOf(items);
        }

        /**
         * A group item starting at the current position, readable as one alphanumeric value. Its
         * elementary items must be declared next; it does not advance the position.
         */
        public AlphanumericField group(String name, int length) {
            checkOpen();
            if (offset + length > limit) {
                throw new IllegalArgumentException(name + " overflows its parent");
            }
            AlphanumericField field = new AlphanumericField(copybook, name, "X(" + length + ") GROUP", offset, length, view);
            root.all.add(field);
            return field;
        }

        /** Starts a view that {@code REDEFINES} {@code target}; its fields must fill the target exactly. */
        public Builder redefines(String viewName, Field target) {
            checkOpen();
            if (this != root || target.view() != null) {
                throw new IllegalArgumentException("Only primary fields can be redefined: " + viewName);
            }
            Builder sub = new Builder(root, viewName, target.offset(), target.offset() + target.length());
            root.views.put(viewName, new ArrayList<>());
            root.redefines.add(sub);
            return sub;
        }

        /** Finishes the layout, checking it against the record length the copybook / JCL declares. */
        public RecordLayout build(int expectedLength) {
            checkOpen();
            if (this != root) {
                throw new IllegalStateException("build() is called on the record builder");
            }
            if (offset != expectedLength) {
                throw new IllegalStateException(copybook + " is " + offset + " bytes, expected " + expectedLength);
            }
            for (Builder sub : redefines) {
                if (sub.offset != sub.limit) {
                    throw new IllegalStateException(copybook + " view " + sub.view + " ends at " + sub.offset
                            + ", the redefined item ends at " + sub.limit);
                }
            }
            built = true;
            return new RecordLayout(this);
        }

        private int reserve(String name, int length) {
            checkOpen();
            if (offset + length > limit) {
                throw new IllegalStateException(copybook + " " + name + " overflows view " + view);
            }
            int start = offset;
            offset += length;
            return start;
        }

        private <F extends Field> F add(F field) {
            root.all.add(field);
            if (view == null) {
                root.primary.add(field);
            } else {
                root.views.get(view).add(field);
            }
            return field;
        }

        private void checkOpen() {
            if (root.built) {
                throw new IllegalStateException(copybook + " layout is already built");
            }
        }
    }
}
