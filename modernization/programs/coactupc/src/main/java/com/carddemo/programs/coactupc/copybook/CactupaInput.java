package com.carddemo.programs.coactupc.copybook;

import com.carddemo.programs.coactupc.support.Cobol;
import java.util.EnumMap;
import java.util.Map;

/**
 * COACTUP - symbolic input map CACTUPAI as received by EXEC CICS RECEIVE MAP.
 * A field that was not transmitted (null) arrives as LOW-VALUES; a transmitted field is
 * left-justified and blank-padded to its defined length (BMS default JUSTIFY=(LEFT,BLANK)).
 */
public final class CactupaInput {

    private final Map<CactupaField, String> values = new EnumMap<>(CactupaField.class);

    public static CactupaInput empty() {
        return new CactupaInput();
    }

    public CactupaInput with(CactupaField field, String value) {
        values.put(field, value);
        return this;
    }

    /** Value of the xxxxI item. */
    public String get(CactupaField field) {
        String v = values.get(field);
        if (v == null) {
            return Cobol.lowValues(field.length);
        }
        return Cobol.pad(v, field.length);
    }

    public CactupaInput copy() {
        CactupaInput c = new CactupaInput();
        c.values.putAll(values);
        return c;
    }
}
