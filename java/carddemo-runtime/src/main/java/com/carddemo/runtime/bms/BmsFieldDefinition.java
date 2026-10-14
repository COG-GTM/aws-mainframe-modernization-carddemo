package com.carddemo.runtime.bms;

import java.util.List;
import java.util.Map;

/**
 * One DFHMDF field. {@code row}/{@code column} are 1-based and locate the attribute byte; data starts in
 * the next position. {@code name} is null for unnamed (literal) fields.
 */
public record BmsFieldDefinition(String name, int row, int column, int length, List<String> attributes,
                                 String color, String hilight, String initial, Map<String, String> operands) {

    public boolean named() {
        return name != null;
    }

    public boolean hasAttribute(String attribute) {
        return attributes.contains(attribute);
    }

    /** DRK (non-display) fields are rendered blank on the terminal. */
    public boolean dark() {
        return hasAttribute("DRK");
    }

    /** IC: initial cursor position when no symbolic cursor is set. */
    public boolean initialCursor() {
        return hasAttribute("IC");
    }

    public boolean justifyRight() {
        String j = operands.get("JUSTIFY");
        return j != null && j.contains("RIGHT");
    }
}
