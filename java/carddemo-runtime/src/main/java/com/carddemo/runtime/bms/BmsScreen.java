package com.carddemo.runtime.bms;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

/**
 * What one SEND MAP put on the terminal: every field (literal and named) with its displayed value,
 * plus the cursor position.
 *
 * @param cursorField name of the field holding the cursor (null when the cursor is not on a named field)
 */
public record BmsScreen(String map, String mapset, int rows, int columns, boolean erase, List<Field> fields,
                        int cursorRow, int cursorColumn, String cursorField) {

    /**
     * A field as displayed.
     *
     * @param value         displayed data, exactly {@code length} characters (null when not sent: DATAONLY
     *                      and no data). LOW-VALUES are shown as spaces.
     * @param sent          true when the symbolic map supplied the data (otherwise the map INITIAL value)
     * @param attributeByte attribute override from the symbolic map (the "A" byte), 0 when none
     */
    public record Field(String name, int row, int column, int length, String value, boolean sent,
                        List<String> attributes, String color, int attributeByte) {

        /** DRK from the map definition, or from the attribute byte the program sent (when it set one). */
        public boolean dark() {
            if (attributeByte != 0) {
                return Dfhbmsca.isDark(attributeByte);
            }
            return attributes.contains("DRK");
        }
    }

    public Optional<Field> field(String name) {
        return fields.stream().filter(f -> name.equals(f.name())).findFirst();
    }

    /** Displayed value of a named field (null when not sent with DATAONLY). */
    public String value(String name) {
        return field(name).orElseThrow(() -> new IllegalArgumentException("No field " + name + " in " + map)).value();
    }

    /** Convenience: the ERRMSG line, when the map has one. */
    public Optional<String> errorMessage() {
        return field("ERRMSG").map(Field::value);
    }

    /** The screen as {@code rows} lines of {@code columns} characters (attribute bytes and DRK fields blank). */
    public List<String> render() {
        char[][] grid = new char[rows][columns];
        for (char[] line : grid) {
            Arrays.fill(line, ' ');
        }
        for (Field f : fields) {
            if (f.value() == null || f.dark()) {
                continue;
            }
            int pos = (f.row() - 1) * columns + f.column();
            for (int i = 0; i < f.length(); i++) {
                int p = pos + i;
                if (p >= rows * columns) {
                    break;
                }
                char c = f.value().charAt(i);
                grid[p / columns][p % columns] = c < ' ' ? ' ' : c;
            }
        }
        List<String> out = new ArrayList<>(rows);
        for (char[] line : grid) {
            out.add(new String(line));
        }
        return out;
    }
}
