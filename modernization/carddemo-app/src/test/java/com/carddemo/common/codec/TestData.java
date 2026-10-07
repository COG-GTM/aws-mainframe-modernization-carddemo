package com.carddemo.common.codec;

import java.nio.file.Files;
import java.nio.file.Path;

/** Locates the legacy estate (app/, docs/) from the Maven module directory. */
public final class TestData {

    private TestData() {
    }

    public static Path repoRoot() {
        Path p = Path.of("").toAbsolutePath();
        while (p != null && !Files.isDirectory(p.resolve("app/data/EBCDIC"))) {
            p = p.getParent();
        }
        if (p == null) {
            throw new IllegalStateException("cannot find app/data/EBCDIC above " + Path.of("").toAbsolutePath());
        }
        return p;
    }

    public static Path resolve(String relative) {
        return repoRoot().resolve(relative);
    }
}
