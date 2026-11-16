package com.carddemo.legacyio.copybook;

import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.NoSuchElementException;

/**
 * A parsed COBOL copybook: its level-01/77 records with offsets and lengths resolved.
 *
 * @param name copybook member name, e.g. {@code CVACT01Y}
 * @param records the level-01 and level-77 entries in source order
 */
public record Copybook(String name, List<CopybookItem> records) {

    /** Classpath folder the build copies {@code app/cpy} into. */
    public static final String RESOURCE_FOLDER = "copybooks/";

    public Copybook {
        records = List.copyOf(records);
    }

    /** Parses copybook source text. */
    public static Copybook parse(String name, String source) {
        return new Copybook(name, new CopybookParser(name, source).parse());
    }

    /** Loads {@code app/cpy/<member>.cpy} (or {@code .CPY}) from the classpath. */
    public static Copybook load(String member) {
        for (String extension : List.of(".cpy", ".CPY")) {
            try (InputStream in = Copybook.class.getClassLoader().getResourceAsStream(RESOURCE_FOLDER + member + extension)) {
                if (in != null) {
                    return parse(member, new String(in.readAllBytes(), StandardCharsets.ISO_8859_1));
                }
            } catch (IOException e) {
                throw new UncheckedIOException("Cannot read copybook " + member, e);
            }
        }
        throw new NoSuchElementException("Copybook " + member + " is not on the classpath under " + RESOURCE_FOLDER);
    }

    /** The only record of a single-record copybook. */
    public CopybookItem record() {
        if (records.size() != 1) {
            throw new IllegalStateException(name + " defines " + records.size() + " records; name the one you want");
        }
        return records.get(0);
    }

    /** The level-01/77 record with the given name. */
    public CopybookItem record(String recordName) {
        return records.stream()
                .filter(r -> r.name().equals(recordName))
                .findFirst()
                .orElseThrow(() -> new NoSuchElementException(name + " has no record " + recordName));
    }
}
