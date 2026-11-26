package com.carddemo.programs.cocrdupc;

import com.carddemo.programs.cocrdupc.copybook.CcrdupaMap;
import com.carddemo.programs.cocrdupc.copybook.CcrdupaMap.Field;

import java.util.Collections;
import java.util.EnumMap;
import java.util.Map;

/** Response DTO for EXEC CICS SEND MAP('CCRDUPA') MAPSET('COCRDUP') FROM(CCRDUPAO) CURSOR ERASE. */
public record CcrdupaScreen(String mapset, String map, Map<Field, ScreenField> fields) {

    /**
     * One output field: symbolic data (raw, may hold LOW-VALUES), physical-map default data,
     * attribute byte, colour byte, cursor.
     */
    public record ScreenField(String value, String initial, int attribute, int color, boolean cursor) {
        /**
         * Data as the 3270 shows it: BMS sends the physical-map default when the symbolic field
         * starts with LOW-VALUES; remaining LOW-VALUES display as blanks. Right-trimmed.
         */
        public String text() {
            String shown = !value.isEmpty() && value.charAt(0) == Cobol.LOW_VALUE ? initial : value;
            return shown.replace(Cobol.LOW_VALUE, ' ').stripTrailing();
        }
    }

    static CcrdupaScreen of(String mapset, String map, CcrdupaMap symbolic) {
        Map<Field, ScreenField> m = new EnumMap<>(Field.class);
        for (Field f : Field.values()) {
            CcrdupaMap.MapField mf = symbolic.get(f);
            m.put(f, new ScreenField(mf.data(), f.initial(), mf.attribute(), mf.color(), mf.hasCursor()));
        }
        return new CcrdupaScreen(mapset, map, Collections.unmodifiableMap(m));
    }

    public ScreenField field(Field f) {
        return fields.get(f);
    }

    public String text(Field f) {
        return fields.get(f).text();
    }

    public Field cursorField() {
        return fields.entrySet().stream().filter(e -> e.getValue().cursor()).map(Map.Entry::getKey)
                .findFirst().orElse(null);
    }
}
