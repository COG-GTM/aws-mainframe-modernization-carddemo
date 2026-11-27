package com.carddemo.data.catalog;

import com.carddemo.common.cobol.CobolDataException;
import com.carddemo.common.copybook.CopybookParser;
import com.carddemo.common.copybook.RecordLayout;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/** Record layouts parsed from the copybooks packaged under {@code copybooks/} (copied from app/cpy at build time). */
public final class CopybookCatalog {

    private static final Map<String, RecordLayout> CACHE = new ConcurrentHashMap<>();

    private CopybookCatalog() {
    }

    public static RecordLayout layout(String member) {
        return CACHE.computeIfAbsent(member, CopybookCatalog::load);
    }

    private static RecordLayout load(String member) {
        String resource = "copybooks/" + member + ".cpy";
        try (InputStream in = CopybookCatalog.class.getClassLoader().getResourceAsStream(resource)) {
            if (in == null) {
                throw new CobolDataException("Copybook resource not found: " + resource);
            }
            return CopybookParser.parse(member, new String(in.readAllBytes(), StandardCharsets.ISO_8859_1)).first();
        } catch (IOException e) {
            throw new CobolDataException("Cannot read " + resource, e);
        }
    }
}
