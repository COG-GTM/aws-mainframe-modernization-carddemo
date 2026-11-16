package com.carddemo.legacyio;

import java.util.List;

/**
 * One data description entry of a copybook: a group (with children) or an elementary item (with a picture).
 *
 * @param name      data name, or {@code FILLER}
 * @param redefines name of the sibling this item redefines, or {@code null}
 * @param occurs    OCCURS count, 1 when absent
 */
public record CopybookItem(
        int level, String name, Picture picture, Usage usage, String redefines, int occurs, List<CopybookItem> children) {

    public CopybookItem {
        children = List.copyOf(children);
    }

    public boolean isGroup() {
        return picture == null;
    }

    /** Bytes one occurrence of this item occupies; a REDEFINES child shares its target's bytes and adds nothing. */
    public int occurrenceLength() {
        if (!isGroup()) {
            return picture.byteLength(usage);
        }
        return children.stream().filter(c -> c.redefines() == null).mapToInt(CopybookItem::totalLength).sum();
    }

    public int totalLength() {
        return occurrenceLength() * occurs;
    }
}
