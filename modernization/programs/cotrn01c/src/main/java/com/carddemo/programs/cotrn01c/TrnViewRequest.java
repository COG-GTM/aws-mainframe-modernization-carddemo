package com.carddemo.programs.cotrn01c;

import com.carddemo.programs.cotrn01c.copybook.Cotrn1a.Field;

import java.util.EnumMap;
import java.util.Map;

/**
 * Input DTO for EXEC CICS RECEIVE MAP('COTRN1A'): the field values returned by the terminal.
 * A field that was not transmitted (null / absent) arrives as LOW-VALUES, as BMS leaves it.
 */
public record TrnViewRequest(Map<Field, String> fields) {

    public TrnViewRequest {
        Map<Field, String> transmitted = new EnumMap<>(Field.class);
        if (fields != null) {
            fields.forEach((f, v) -> {
                if (f != null && v != null) {
                    transmitted.put(f, v);
                }
            });
        }
        fields = Map.copyOf(transmitted);
    }

    /** Only the "Enter Tran ID" (TRNIDIN, the sole unprotected field) entered. */
    public static TrnViewRequest ofTranId(String trnidin) {
        Map<Field, String> m = new EnumMap<>(Field.class);
        if (trnidin != null) {
            m.put(Field.TRNIDIN, trnidin);
        }
        return new TrnViewRequest(m);
    }

    /** Nothing transmitted (e.g. MAPFAIL). */
    public static TrnViewRequest empty() {
        return new TrnViewRequest(Map.of());
    }
}
