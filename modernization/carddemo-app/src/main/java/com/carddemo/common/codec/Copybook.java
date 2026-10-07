package com.carddemo.common.codec;

import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Locale;

/**
 * A parsed COBOL data copybook: one {@link RecordLayout} per {@code 01}/{@code 77} entry. A copybook that
 * starts below level 01 (a fragment such as {@code CSUTLDWY}) yields a single layout named after the copybook.
 *
 * <p>The legacy copybooks from {@code app/cpy} are packaged on the classpath under {@code copybooks/}, so
 * {@link #load(String)} reads the same layouts the COBOL programs compile against.
 */
public final class Copybook {

    private final String name;
    private final List<RecordLayout> records;

    private Copybook(String name, List<RecordLayout> records) {
        this.name = name;
        this.records = List.copyOf(records);
    }

    public static Copybook parse(String name, String source) {
        return new Copybook(name, CopybookParser.parse(name, source));
    }

    /** Loads {@code copybooks/<name>.cpy} (or {@code .CPY}) from the classpath. */
    public static Copybook load(String name) {
        String base = name.toUpperCase(Locale.ROOT);
        for (String resource : List.of(base + ".cpy", base + ".CPY")) {
            try (InputStream in = Copybook.class.getClassLoader().getResourceAsStream("copybooks/" + resource)) {
                if (in != null) {
                    return parse(base, new String(in.readAllBytes(), StandardCharsets.ISO_8859_1));
                }
            } catch (IOException e) {
                throw new UncheckedIOException("cannot read copybook " + resource, e);
            }
        }
        throw new IllegalArgumentException("no copybook " + name + " on the classpath");
    }

    /** Shorthand for the single record of a one-record copybook. */
    public static RecordLayout layout(String name) {
        return load(name).single();
    }

    public String name() {
        return name;
    }

    public List<RecordLayout> records() {
        return records;
    }

    public RecordLayout record(String recordName) {
        return records.stream().filter(r -> r.name().equalsIgnoreCase(recordName)).findFirst()
                .orElseThrow(() -> new IllegalArgumentException(name + " has no record " + recordName));
    }

    public RecordLayout single() {
        if (records.size() != 1) {
            throw new IllegalStateException(name + " has " + records.size() + " records");
        }
        return records.get(0);
    }
}
