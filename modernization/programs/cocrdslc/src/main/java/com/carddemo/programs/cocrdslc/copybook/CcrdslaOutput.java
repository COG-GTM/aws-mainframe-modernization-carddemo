package com.carddemo.programs.cocrdslc.copybook;

import java.util.EnumMap;
import java.util.Map;
import java.util.Optional;

/** COCRDSL - CCRDSLAO symbolic output map (response DTO for EXEC CICS SEND MAP('CCRDSLA')). */
public final class CcrdslaOutput {
    private final Map<CcrdslaField, MapField> fields = new EnumMap<>(CcrdslaField.class);

    /** MOVE LOW-VALUES TO CCRDSLAO. */
    public CcrdslaOutput() {
        for (CcrdslaField f : CcrdslaField.values()) {
            fields.put(f, new MapField(f));
        }
    }

    public MapField field(CcrdslaField f) {
        return fields.get(f);
    }

    public String data(CcrdslaField f) {
        return fields.get(f).data();
    }

    /** SEND MAP ... CURSOR: the first field (map order) whose L is -1. */
    public Optional<CcrdslaField> cursor() {
        for (CcrdslaField f : CcrdslaField.values()) {
            if (fields.get(f).length() == -1) {
                return Optional.of(f);
            }
        }
        return Optional.empty();
    }
}
