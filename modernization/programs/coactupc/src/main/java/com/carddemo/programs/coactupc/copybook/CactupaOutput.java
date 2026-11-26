package com.carddemo.programs.coactupc.copybook;

import com.carddemo.programs.coactupc.support.Cobol;
import java.util.EnumMap;
import java.util.Map;

/**
 * COACTUP - symbolic output map CACTUPAO (REDEFINES CACTUPAI) as sent by EXEC CICS SEND MAP.
 * Holds, per field, the xxxxO data, the xxxxA attribute byte, the xxxxC colour byte and whether
 * xxxxL was set to -1 (symbolic cursor positioning). A null attribute/colour means LOW-VALUES,
 * i.e. the physical map default is used.
 */
public final class CactupaOutput {

    private final Map<CactupaField, String> data = new EnumMap<>(CactupaField.class);
    private final Map<CactupaField, BmsAttribute> attributes = new EnumMap<>(CactupaField.class);
    private final Map<CactupaField, BmsColor> colors = new EnumMap<>(CactupaField.class);
    private final Map<CactupaField, Boolean> cursor = new EnumMap<>(CactupaField.class);
    private String mapName = "";
    private String mapsetName = "";
    private boolean erase;
    private boolean freeKb;

    /** MOVE LOW-VALUES TO CACTUPAO. */
    public static CactupaOutput lowValues() {
        return new CactupaOutput();
    }

    public void setData(CactupaField field, String value) {
        data.put(field, Cobol.pad(value, field.length));
    }

    public void setLowValues(CactupaField field) {
        data.remove(field);
    }

    /** xxxxO value (LOW-VALUES when never moved). */
    public String data(CactupaField field) {
        String v = data.get(field);
        return v == null ? Cobol.lowValues(field.length) : v;
    }

    /** xxxxO value with LOW-VALUES and trailing spaces removed, as a terminal would display it. */
    public String text(CactupaField field) {
        return data(field).replace(Cobol.LOW_VALUE, ' ').stripTrailing();
    }

    public void setAttribute(CactupaField field, BmsAttribute attribute) {
        attributes.put(field, attribute);
    }

    public BmsAttribute attribute(CactupaField field) {
        return attributes.get(field);
    }

    public void setColor(CactupaField field, BmsColor color) {
        colors.put(field, color);
    }

    public BmsColor color(CactupaField field) {
        return colors.get(field);
    }

    /** MOVE -1 TO xxxxL. */
    public void setCursor(CactupaField field) {
        cursor.put(field, Boolean.TRUE);
    }

    /** Field where the cursor is placed by SEND MAP ... CURSOR (first field with length -1). */
    public CactupaField cursorField() {
        for (CactupaField f : CactupaField.values()) {
            if (cursor.containsKey(f)) {
                return f;
            }
        }
        return null;
    }

    public void sent(String map, String mapset, boolean eraseScreen, boolean freeKeyboard) {
        this.mapName = map;
        this.mapsetName = mapset;
        this.erase = eraseScreen;
        this.freeKb = freeKeyboard;
    }

    public String mapName() {
        return mapName;
    }

    public String mapsetName() {
        return mapsetName;
    }

    public boolean erase() {
        return erase;
    }

    public boolean freeKb() {
        return freeKb;
    }
}
