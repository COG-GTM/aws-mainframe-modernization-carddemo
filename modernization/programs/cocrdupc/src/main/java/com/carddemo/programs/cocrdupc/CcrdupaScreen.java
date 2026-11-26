package com.carddemo.programs.cocrdupc;

import com.carddemo.programs.cocrdupc.copybook.CcrdupaMap;
import com.carddemo.programs.cocrdupc.copybook.CcrdupaMap.Field;

import java.util.Collections;
import java.util.EnumMap;
import java.util.Map;

/** Response DTO for EXEC CICS SEND MAP('CCRDUPA') MAPSET('COCRDUP') FROM(CCRDUPAO) CURSOR ERASE. */
public record CcrdupaScreen(String mapset, String map, Map<Field, ScreenField> fields) {

    /** One output field: data (raw, may hold LOW-VALUES), attribute byte, colour byte, cursor. */
    public record ScreenField(String value, int attribute, int color, boolean cursor) {
        /** Data as a 3270 would display it (LOW-VALUES shown as blanks), right-trimmed. */
        public String text() {
            return value.replace(Cobol.LOW_VALUE, ' ').stripTrailing();
        }
    }

    static CcrdupaScreen of(String mapset, String map, CcrdupaMap symbolic) {
        Map<Field, ScreenField> m = new EnumMap<>(Field.class);
        for (Field f : Field.values()) {
            CcrdupaMap.MapField mf = symbolic.get(f);
            m.put(f, new ScreenField(mf.data(), mf.attribute(), mf.color(), mf.hasCursor()));
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
