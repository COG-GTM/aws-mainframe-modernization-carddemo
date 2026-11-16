package com.carddemo.legacyio.copybook;

import java.util.List;
import java.util.Optional;

/**
 * One data description entry of a copybook: a group (with children) or an elementary item (with a picture).
 *
 * @param level level number (01-49, 77)
 * @param name data name, {@code FILLER} when unnamed
 * @param picture picture of an elementary item, {@code null} for a group
 * @param usage storage format (inherited from the enclosing group when not written)
 * @param occurs repeat count, 1 when there is no {@code OCCURS}
 * @param redefines name of the item this one redefines, {@code null} when none
 * @param offset byte offset of the first occurrence from the start of the 01 record
 * @param length bytes of one occurrence
 * @param children subordinate items of a group, empty for an elementary item
 */
public record CopybookItem(
        int level,
        String name,
        Picture picture,
        Usage usage,
        int occurs,
        String redefines,
        int offset,
        int length,
        List<CopybookItem> children) {

    public CopybookItem {
        children = List.copyOf(children);
    }

    public boolean isGroup() {
        return picture == null;
    }

    public boolean isFiller() {
        return "FILLER".equals(name);
    }

    /** Bytes taken by all occurrences. */
    public int totalLength() {
        return length * occurs;
    }

    /** Finds this item or a descendant by name (first match, depth first). */
    public Optional<CopybookItem> find(String itemName) {
        if (name.equals(itemName)) {
            return Optional.of(this);
        }
        for (CopybookItem child : children) {
            Optional<CopybookItem> found = child.find(itemName);
            if (found.isPresent()) {
                return found;
            }
        }
        return Optional.empty();
    }
}
