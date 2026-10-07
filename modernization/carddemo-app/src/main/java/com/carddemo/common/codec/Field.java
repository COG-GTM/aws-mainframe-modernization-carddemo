package com.carddemo.common.codec;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

/**
 * A data item of a copybook record: its byte offset in the record, its size and storage format.
 *
 * <p>Items inside {@code OCCURS} tables describe their first occurrence; {@link #subscript(int...)} returns the
 * item relocated to a given occurrence using COBOL's 1-based subscripts, one per enclosing table (outermost
 * first). Items that {@code REDEFINES} another start at the redefined item's offset.
 */
public final class Field {

    private final String name;
    private final int level;
    private final int offset;
    private final int size;
    private final Picture picture;
    private final Usage usage;
    private final int occurs;
    private final String dependingOn;
    private final String redefines;
    private final List<Field> children;
    private final int[] strides;
    private final int[] counts;
    private final List<String> ancestors;

    Field(String name, int level, int offset, int size, Picture picture, Usage usage, int occurs,
          String dependingOn, String redefines, List<Field> children, int[] strides, int[] counts,
          List<String> ancestors) {
        this.name = name;
        this.level = level;
        this.offset = offset;
        this.size = size;
        this.picture = picture;
        this.usage = usage;
        this.occurs = occurs;
        this.dependingOn = dependingOn;
        this.redefines = redefines;
        this.children = List.copyOf(children);
        this.strides = strides;
        this.counts = counts;
        this.ancestors = List.copyOf(ancestors);
    }

    public String name() {
        return name;
    }

    public int level() {
        return level;
    }

    /** 0-based byte offset in the record. */
    public int offset() {
        return offset;
    }

    /** Bytes of one occurrence. */
    public int size() {
        return size;
    }

    /** Bytes of all occurrences ({@code size() * occurs()}). */
    public int totalSize() {
        return size * Math.max(occurs, 1);
    }

    /** The picture, or {@code null} for a group. */
    public Picture picture() {
        return picture;
    }

    public Usage usage() {
        return usage;
    }

    /** The (maximum) {@code OCCURS} count, 0 without an OCCURS clause. */
    public int occurs() {
        return occurs;
    }

    public String dependingOn() {
        return dependingOn;
    }

    /** Name of the item this one redefines, or {@code null}. */
    public String redefines() {
        return redefines;
    }

    public List<Field> children() {
        return children;
    }

    /** Names of the enclosing groups, nearest first. */
    public List<String> ancestors() {
        return ancestors;
    }

    public boolean isGroup() {
        return picture == null;
    }

    public boolean isFiller() {
        return "FILLER".equalsIgnoreCase(name);
    }

    public boolean isNumeric() {
        return picture != null && picture.isNumeric();
    }

    public boolean isNumericEdited() {
        return picture != null && picture.category() == Picture.Category.NUMERIC_EDITED;
    }

    public int digits() {
        return picture == null ? 0 : picture.digits();
    }

    public int scale() {
        return picture == null ? 0 : picture.scale();
    }

    public boolean signed() {
        return picture != null && picture.signed();
    }

    /** Number of subscripts this item needs (enclosing tables, including its own OCCURS). */
    public int dimensions() {
        return strides.length;
    }

    /** This item relocated to the given occurrence; one 1-based subscript per enclosing table, outermost first. */
    public Field subscript(int... subscripts) {
        if (subscripts.length != strides.length) {
            throw new IllegalArgumentException(name + " needs " + strides.length + " subscript(s), got "
                    + subscripts.length);
        }
        int delta = 0;
        for (int i = 0; i < subscripts.length; i++) {
            if (subscripts[i] < 1 || subscripts[i] > counts[i]) {
                throw new IndexOutOfBoundsException(name + " subscript " + subscripts[i] + " outside 1.."
                        + counts[i]);
            }
            delta += (subscripts[i] - 1) * strides[i];
        }
        return relocate(delta, subscripts.length);
    }

    /** The first descendant named {@code childName} (depth first), in this item's coordinates. */
    public Field child(String childName) {
        Field found = find(childName);
        if (found == null) {
            throw new IllegalArgumentException(name + " has no subordinate item " + childName);
        }
        return found;
    }

    private Field find(String childName) {
        for (Field c : children) {
            if (c.name.equalsIgnoreCase(childName)) {
                return c;
            }
            Field below = c.find(childName);
            if (below != null) {
                return below;
            }
        }
        return null;
    }

    private Field relocate(int delta, int consumed) {
        List<Field> moved = new ArrayList<>(children.size());
        for (Field c : children) {
            moved.add(c.relocate(delta, consumed));
        }
        return new Field(name, level, offset + delta, size, picture, usage, occurs, dependingOn, redefines, moved,
                Arrays.copyOfRange(strides, consumed, strides.length),
                Arrays.copyOfRange(counts, consumed, counts.length), ancestors);
    }

    @Override
    public boolean equals(Object o) {
        return o instanceof Field f && f.name.equals(name) && f.offset == offset && f.size == size
                && f.level == level && Objects.equals(f.picture, picture) && f.usage == usage
                && f.occurs == occurs && Arrays.equals(f.strides, strides);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name, offset, size, level);
    }

    @Override
    public String toString() {
        return String.format("%02d %s @%d+%d%s%s%s", level, name, offset, size,
                picture == null ? "" : " PIC " + picture.text(),
                usage == Usage.DISPLAY ? "" : " " + usage,
                occurs > 0 ? " OCCURS " + occurs : "");
    }
}
