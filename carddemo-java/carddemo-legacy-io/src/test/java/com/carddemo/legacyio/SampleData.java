package com.carddemo.legacyio;

import java.nio.file.Files;
import java.nio.file.Path;

/** Locates the COBOL application's {@code app/} directory (copybooks and sample data). */
final class SampleData {

    private SampleData() {
    }

    static Path appDir() {
        String configured = System.getProperty("carddemo.app.dir");
        Path dir = configured != null ? Path.of(configured) : Path.of("..", "..", "app");
        if (!Files.isDirectory(dir.resolve("cpy"))) {
            throw new IllegalStateException("CardDemo app directory not found at " + dir.toAbsolutePath());
        }
        return dir.normalize();
    }

    static Path ascii(String file) {
        return appDir().resolve("data/ASCII").resolve(file);
    }

    static Path ebcdic(String file) {
        return appDir().resolve("data/EBCDIC").resolve(file);
    }
}
