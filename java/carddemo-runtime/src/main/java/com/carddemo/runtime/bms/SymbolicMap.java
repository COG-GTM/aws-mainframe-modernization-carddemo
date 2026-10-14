package com.carddemo.runtime.bms;

import com.carddemo.runtime.cobol.PicX;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * The symbolic map (the xxxxI / xxxxO copybook from app/cpy-bms) of one BMS map. As in COBOL the input
 * and output views share storage: {@code data(name)} is both {@code nameI} and {@code nameO}, so data
 * received from the terminal is echoed back by a later SEND MAP unless the program overwrites it.
 *
 * <p>Per named field it holds: L ({@link #length}, -1 = symbolic cursor), F/A ({@link #flag}),
 * extended attributes C/H, and the data. A new instance is all LOW-VALUES (uninitialised
 * WORKING-STORAGE convention; see README).
 */
public final class SymbolicMap {

    /** RECEIVE MAP flag byte for a field that was modified but cleared (erased to nulls). */
    public static final int FLAG_CLEARED = 0x80;

    private final BmsMapDefinition definition;
    private final Map<String, Slot> slots = new LinkedHashMap<>();

    private static final class Slot {
        final BmsFieldDefinition def;
        int length;
        int flag;
        int color;
        int hilight;
        String data;

        Slot(BmsFieldDefinition def) {
            this.def = def;
            this.data = PicX.lowValues(def.length());
        }
    }

    public SymbolicMap(BmsMapDefinition definition) {
        this.definition = definition;
        for (BmsFieldDefinition f : definition.namedFields()) {
            slots.put(f.name(), new Slot(f));
        }
    }

    public static SymbolicMap of(String mapset, String map) {
        return new SymbolicMap(BmsMapsetRegistry.map(mapset, map));
    }

    public BmsMapDefinition definition() {
        return definition;
    }

    /** MOVE LOW-VALUES TO mapO (whole symbolic map). */
    public void lowValues() {
        for (Slot s : slots.values()) {
            s.length = 0;
            s.flag = 0;
            s.color = 0;
            s.hilight = 0;
            s.data = PicX.lowValues(s.def.length());
        }
    }

    /** {@code nameI} / {@code nameO}. */
    public String data(String name) {
        return slot(name).data;
    }

    /** MOVE value TO nameO (pad with spaces / truncate to the field length). */
    public void setData(String name, String value) {
        Slot s = slot(name);
        s.data = PicX.of(value, s.def.length());
    }

    /** Overwrites only the leading {@code value.length()} bytes (e.g. ASSIGN SYSID into a longer field). */
    public void setDataPrefix(String name, String value) {
        Slot s = slot(name);
        int n = Math.min(value.length(), s.def.length());
        s.data = value.substring(0, n) + s.data.substring(n);
    }

    /** {@code nameL}. */
    public int length(String name) {
        return slot(name).length;
    }

    /** MOVE n TO nameL ({@code -1} requests the cursor on this field with SEND MAP ... CURSOR). */
    public void setLength(String name, int value) {
        slot(name).length = (short) value;
    }

    /** {@code MOVE -1 TO nameL}. */
    public void setCursor(String name) {
        setLength(name, -1);
    }

    /** {@code nameF} (input flag) / {@code nameA} (output attribute) - same byte. */
    public int flag(String name) {
        return slot(name).flag;
    }

    public void setAttribute(String name, int attributeByte) {
        slot(name).flag = attributeByte & 0xFF;
    }

    public void setColor(String name, int colorByte) {
        slot(name).color = colorByte & 0xFF;
    }

    /**
     * BMS COLOR name for a DFHBMSCA extended-colour byte moved into {@code nameC}; 0 (not set / DFHDFCOL) and
     * non-colour bytes keep the physical map colour.
     */
    static String colorName(int colorByte, String physical) {
        return switch (colorByte) {
            case 0xF1 -> "BLUE";
            case 0xF2 -> "RED";
            case 0xF3 -> "PINK";
            case 0xF4 -> "GREEN";
            case 0xF5 -> "TURQUOISE";
            case 0xF6 -> "YELLOW";
            case 0xF7 -> "NEUTRAL";
            default -> physical;
        };
    }

    /** {@code nameC} (extended colour byte set by the program; 0 = DFHDFCOL / not set). */
    public int color(String name) {
        return slot(name).color;
    }

    /**
     * RECEIVE MAP: for each named field, if the terminal transmitted it, data = text left-justified and
     * space-padded (right-justified for JUSTIFY=RIGHT), truncated to the field length, L = data length;
     * a transmitted empty value sets L = 0, F = X'80' and data to LOW-VALUES. Fields not transmitted get
     * L = 0 and LOW-VALUES. Unknown field names are rejected.
     */
    public void receive(Map<String, String> input) {
        for (String key : input.keySet()) {
            if (!slots.containsKey(key)) {
                throw new IllegalArgumentException("Map " + definition.name() + " has no field " + key);
            }
        }
        for (Slot s : slots.values()) {
            String v = input.get(s.def.name());
            int len = s.def.length();
            s.flag = 0;
            if (v == null) {
                s.length = 0;
                s.data = PicX.lowValues(len);
            } else if (v.isEmpty()) {
                s.length = 0;
                s.flag = FLAG_CLEARED;
                s.data = PicX.lowValues(len);
            } else {
                String t = v.length() > len ? v.substring(0, len) : v;
                s.length = t.length();
                s.data = s.def.justifyRight() ? PicX.justifiedRight(t, len) : PicX.of(t, len);
            }
        }
    }

    /**
     * Produces what SEND MAP shows. Without DATAONLY the physical map is written first, so a field whose
     * symbolic data starts with LOW-VALUE shows its INITIAL value (or blanks); with MAPONLY no symbolic
     * data is used. With CURSOR the cursor goes to the first field whose L is -1, else the IC field.
     */
    public BmsScreen render(boolean erase, boolean cursor, boolean mapOnly, boolean dataOnly) {
        List<BmsScreen.Field> out = new ArrayList<>();
        for (BmsFieldDefinition f : definition.fields()) {
            Slot s = f.named() ? slots.get(f.name()) : null;
            boolean sent = !mapOnly && s != null && !s.data.isEmpty() && s.data.charAt(0) != PicX.LOW_VALUE;
            String value;
            if (sent) {
                value = s.data.replace(PicX.LOW_VALUE, ' ');
            } else if (dataOnly) {
                value = null;
            } else {
                value = PicX.of(f.initial() == null ? "" : f.initial(), f.length());
            }
            int attr = s == null || mapOnly ? 0 : s.flag;
            String color = s == null || mapOnly ? f.color() : colorName(s.color, f.color());
            out.add(new BmsScreen.Field(f.name(), f.row(), f.column(), f.length(), value, sent, f.attributes(),
                    color, attr));
        }
        BmsFieldDefinition cursorAt = null;
        if (cursor && !mapOnly) {
            cursorAt = slots.values().stream().filter(s -> s.length == -1).map(s -> s.def).findFirst().orElse(null);
        }
        if (cursorAt == null) {
            cursorAt = definition.fields().stream().filter(BmsFieldDefinition::initialCursor).findFirst().orElse(null);
        }
        int row = 1;
        int col = 1;
        String cursorName = null;
        if (cursorAt != null) {
            int pos = (cursorAt.row() - 1) * definition.columns() + cursorAt.column();
            row = pos / definition.columns() + 1;
            col = pos % definition.columns() + 1;
            cursorName = cursorAt.name();
        }
        return new BmsScreen(definition.name(), definition.mapset(), definition.rows(), definition.columns(), erase,
                List.copyOf(out), row, col, cursorName);
    }

    private Slot slot(String name) {
        Slot s = slots.get(name);
        if (s == null) {
            throw new IllegalArgumentException("Map " + definition.name() + " has no field " + name);
        }
        return s;
    }
}
