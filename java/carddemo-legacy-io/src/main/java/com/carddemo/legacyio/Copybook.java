package com.carddemo.legacyio;

import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

/**
 * A parsed COBOL copybook: its record descriptions (01 levels). The CardDemo copybooks in {@code app/cpy} are packaged
 * on the classpath under {@code copybooks/}, so {@code Copybook.load("CVACT01Y")} parses {@code app/cpy/CVACT01Y.cpy}.
 */
public record Copybook(String name, List<CopybookItem> records) {

    public Copybook {
        records = List.copyOf(records);
    }

    public static Copybook parse(String name, String source) {
        return new Copybook(name, CopybookParser.parse(name, source));
    }

    public static Copybook load(String name) {
        for (String file : List.of(name + ".cpy", name + ".CPY")) {
            try (InputStream in = Copybook.class.getResourceAsStream("/copybooks/" + file)) {
                if (in != null) {
                    return parse(name, new String(in.readAllBytes(), StandardCharsets.ISO_8859_1));
                }
            } catch (IOException e) {
                throw new UncheckedIOException(e);
            }
        }
        throw new IllegalArgumentException("copybook " + name + " is not on the classpath under copybooks/");
    }

    public static Copybook read(Path file) throws IOException {
        String fileName = file.getFileName().toString();
        int dot = fileName.lastIndexOf('.');
        return parse(dot < 0 ? fileName : fileName.substring(0, dot),
                Files.readString(file, StandardCharsets.ISO_8859_1));
    }

    /** Layout of the only record in this copybook. */
    public RecordLayout layout() {
        if (records.size() != 1) {
            throw new IllegalStateException("copybook " + name + " has " + records.size() + " records; name one");
        }
        return RecordLayout.of(records.get(0));
    }

    public RecordLayout layout(String recordName) {
        return records.stream()
                .filter(r -> r.name().equalsIgnoreCase(recordName))
                .findFirst()
                .map(RecordLayout::of)
                .orElseThrow(() -> new IllegalArgumentException("copybook " + name + " has no record " + recordName));
    }
}
