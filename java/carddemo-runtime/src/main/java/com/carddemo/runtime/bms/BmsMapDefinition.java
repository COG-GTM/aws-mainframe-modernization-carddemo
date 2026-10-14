package com.carddemo.runtime.bms;

import java.util.List;
import java.util.Map;
import java.util.Optional;

/** One DFHMDI map of a mapset. */
public record BmsMapDefinition(String name, String mapset, int rows, int columns, List<BmsFieldDefinition> fields,
                               Map<String, String> operands) {

    public List<BmsFieldDefinition> namedFields() {
        return fields.stream().filter(BmsFieldDefinition::named).toList();
    }

    public Optional<BmsFieldDefinition> field(String fieldName) {
        return fields.stream().filter(f -> fieldName.equals(f.name())).findFirst();
    }
}
