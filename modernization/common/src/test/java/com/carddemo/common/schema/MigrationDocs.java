package com.carddemo.common.schema;

import java.nio.file.Files;
import java.nio.file.Path;

/** Locates the repository's {@code app/} reference sources and {@code docs/migration/} from a module. */
public final class MigrationDocs {

    private MigrationDocs() {
    }

    public static Path repoRoot() {
        Path dir = Path.of("").toAbsolutePath();
        while (dir != null) {
            if (Files.isDirectory(dir.resolve("app/cpy")) && Files.isDirectory(dir.resolve("docs/migration"))) {
                return dir;
            }
            dir = dir.getParent();
        }
        throw new IllegalStateException("repository root (app/cpy + docs/migration) not found above "
                + Path.of("").toAbsolutePath());
    }

    public static Path schemaMapping() {
        return repoRoot().resolve("docs/migration/schema-mapping.md");
    }

    public static Path copybook(String name) {
        return repoRoot().resolve("app/cpy/" + name + ".cpy");
    }
}
