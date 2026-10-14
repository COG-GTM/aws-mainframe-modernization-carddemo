package com.carddemo.runtime.bms;

import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Loads mapsets from the classpath ({@code carddemo/bms/<MAPSET>.bms}, copied verbatim from app/bms by the
 * carddemo-runtime build) and caches the parsed definitions.
 */
public final class BmsMapsetRegistry {

    private static final Map<String, BmsMapsetDefinition> CACHE = new ConcurrentHashMap<>();

    private BmsMapsetRegistry() {
    }

    public static BmsMapsetDefinition mapset(String mapsetName) {
        return CACHE.computeIfAbsent(mapsetName.trim(), BmsMapsetRegistry::load);
    }

    public static BmsMapDefinition map(String mapsetName, String mapName) {
        return mapset(mapsetName).map(mapName.trim());
    }

    private static BmsMapsetDefinition load(String name) {
        String resource = "carddemo/bms/" + name + ".bms";
        try (InputStream in = BmsMapsetRegistry.class.getClassLoader().getResourceAsStream(resource)) {
            if (in == null) {
                throw new IllegalArgumentException("BMS mapset not found on classpath: " + resource);
            }
            return BmsParser.parse(new String(in.readAllBytes(), StandardCharsets.ISO_8859_1));
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }
}
