package com.carddemo.programs.coactupc.copybook;

import com.carddemo.programs.coactupc.support.Cobol;
import java.util.EnumMap;
import java.util.EnumSet;
import java.util.Map;
import java.util.Set;

/**
 * COACTUP - symbolic input map CACTUPAI as received by EXEC CICS RECEIVE MAP.
 * A field that was not transmitted (null) arrives as LOW-VALUES; a transmitted field is
 * blank-padded to its defined length: right-justified for the JUSTIFY=(RIGHT) fields of COACTUP.bms
 * (dates and phone parts), otherwise left-justified (BMS default JUSTIFY=(LEFT,BLANK)).
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
        if (RIGHT_JUSTIFIED.contains(field)) {
            String t = v.stripTrailing();
            return t.length() >= field.length ? t.substring(0, field.length) : Cobol.spaces(field.length - t.length()) + t;
        }
        return Cobol.pad(v, field.length);
    }

    private static final Set<CactupaField> RIGHT_JUSTIFIED = EnumSet.of(
            CactupaField.OPNYEAR, CactupaField.OPNMON, CactupaField.OPNDAY,
            CactupaField.EXPYEAR, CactupaField.EXPMON, CactupaField.EXPDAY,
            CactupaField.RISYEAR, CactupaField.RISMON, CactupaField.RISDAY,
            CactupaField.DOBYEAR, CactupaField.DOBMON, CactupaField.DOBDAY,
            CactupaField.ACSPH1A, CactupaField.ACSPH1B, CactupaField.ACSPH1C,
            CactupaField.ACSPH2A, CactupaField.ACSPH2B, CactupaField.ACSPH2C);

    public CactupaInput copy() {
        CactupaInput c = new CactupaInput();
        c.values.putAll(values);
        return c;
    }
}
