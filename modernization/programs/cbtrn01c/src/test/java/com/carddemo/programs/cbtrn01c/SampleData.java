package com.carddemo.programs.cbtrn01c;

import java.nio.file.Files;
import java.nio.file.Path;

/** Locates the repository's app/data/ASCII sample datasets from the module directory. */
public final class SampleData {

    private SampleData() {
    }

    public static Path dir() {
        Path p = Path.of(System.getProperty("user.dir")).toAbsolutePath();
        while (p != null) {
            Path candidate = p.resolve("app/data/ASCII");
            if (Files.isDirectory(candidate)) {
                return candidate;
            }
            p = p.getParent();
        }
        return null;
    }

    public static Path file(String name) {
        Path d = dir();
        return d == null ? null : d.resolve(name);
    }
}
