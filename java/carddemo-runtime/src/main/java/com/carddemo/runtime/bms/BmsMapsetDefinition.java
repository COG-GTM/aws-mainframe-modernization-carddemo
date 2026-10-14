package com.carddemo.runtime.bms;

import java.util.List;
import java.util.Map;

/** A DFHMSD mapset with its maps. */
public record BmsMapsetDefinition(String name, Map<String, String> operands, List<BmsMapDefinition> maps) {

    public BmsMapDefinition map(String mapName) {
        return maps.stream().filter(m -> m.name().equals(mapName)).findFirst()
                .orElseThrow(() -> new IllegalArgumentException("Mapset " + name + " has no map " + mapName));
    }
}
