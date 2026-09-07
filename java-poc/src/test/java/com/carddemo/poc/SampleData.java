package com.carddemo.poc;

import java.nio.file.Files;
import java.nio.file.Path;

/** Locates the CardDemo sample data shipped in {@code app/data} of the repository. */
public final class SampleData {

    private SampleData() {
    }

    public static Path dataDir() {
        Path dir = Path.of(System.getProperty("carddemo.data.dir", "../app/data")).toAbsolutePath().normalize();
        if (!Files.isDirectory(dir)) {
            throw new IllegalStateException("CardDemo sample data not found at " + dir
                    + " (set -Dcarddemo.data.dir=<repo>/app/data)");
        }
        return dir;
    }

    public static Path ebcdic(String name) {
        return dataDir().resolve("EBCDIC").resolve(name);
    }

    public static Path ascii(String name) {
        return dataDir().resolve("ASCII").resolve(name);
    }
}
