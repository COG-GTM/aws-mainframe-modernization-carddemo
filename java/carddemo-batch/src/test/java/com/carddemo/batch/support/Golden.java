package com.carddemo.batch.support;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.google.gson.JsonPrimitive;

import java.io.IOException;
import java.io.Reader;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Reads the golden JSON record sets written by {@code test-harness/records.py}. Scalars stay as the exact
 * JSON strings (numbers are decimal strings such as {@code "-1025.00"}, text keeps its trailing spaces);
 * integers such as {@code _length} become {@link Integer}; OCCURS groups become lists of maps.
 */
public final class Golden {

    private Golden() {
    }

    @SuppressWarnings("unchecked")
    public static List<Map<String, Object>> records(Path json) {
        try (Reader r = Files.newBufferedReader(json, StandardCharsets.UTF_8)) {
            return (List<Map<String, Object>>) convert(JsonParser.parseReader(r));
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    public static Map<String, Object> object(Path json) {
        try (Reader r = Files.newBufferedReader(json, StandardCharsets.UTF_8)) {
            @SuppressWarnings("unchecked")
            Map<String, Object> m = (Map<String, Object>) convert(JsonParser.parseReader(r));
            return m;
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    private static Object convert(JsonElement e) {
        if (e.isJsonArray()) {
            List<Object> out = new ArrayList<>();
            for (JsonElement x : (JsonArray) e) {
                out.add(convert(x));
            }
            return out;
        }
        if (e.isJsonObject()) {
            Map<String, Object> out = new LinkedHashMap<>();
            for (Map.Entry<String, JsonElement> en : ((JsonObject) e).entrySet()) {
                out.put(en.getKey(), convert(en.getValue()));
            }
            return out;
        }
        if (e.isJsonNull()) {
            return null;
        }
        JsonPrimitive p = e.getAsJsonPrimitive();
        if (p.isNumber()) {
            return p.getAsNumber().intValue();
        }
        if (p.isBoolean()) {
            return p.getAsBoolean();
        }
        return p.getAsString();
    }
}
