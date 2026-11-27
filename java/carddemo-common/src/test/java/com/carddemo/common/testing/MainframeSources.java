package com.carddemo.common.testing;

import com.carddemo.common.copybook.Copybook;
import com.carddemo.common.copybook.CopybookParser;
import com.carddemo.common.copybook.RecordLayout;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.Locale;
import java.util.Optional;
import java.util.stream.Stream;

/**
 * Locates the original mainframe artefacts (copybooks, sample datasets) of the repository from tests.
 * The root is the {@code carddemo.repo.root} system property (set by the Maven build) or the first parent of the
 * working directory that contains {@code app/cpy}.
 */
public final class MainframeSources {

    /** Copybook directories searched in order: core, BMS symbolic maps, then the optional modules. */
    private static final List<String> COPYBOOK_DIRS = List.of("app/cpy", "app/cpy-bms",
            "app/app-authorization-ims-db2-mq/cpy", "app/app-authorization-ims-db2-mq/cpy-bms",
            "app/app-transaction-type-db2/cpy", "app/app-transaction-type-db2/cpy-bms");

    private final Path root;

    public MainframeSources(Path root) {
        this.root = root.toAbsolutePath().normalize();
        if (!Files.isDirectory(this.root.resolve("app/cpy"))) {
            throw new IllegalStateException("Not a CardDemo repository root (no app/cpy): " + this.root);
        }
    }

    public static MainframeSources locate() {
        String property = System.getProperty("carddemo.repo.root");
        if (property != null && Files.isDirectory(Paths.get(property, "app", "cpy"))) {
            return new MainframeSources(Paths.get(property));
        }
        for (Path p = Paths.get("").toAbsolutePath(); p != null; p = p.getParent()) {
            if (Files.isDirectory(p.resolve("app/cpy"))) {
                return new MainframeSources(p);
            }
        }
        throw new IllegalStateException("Cannot locate the CardDemo repository root; set -Dcarddemo.repo.root");
    }

    public Path root() {
        return root;
    }

    public Path resolve(String relative) {
        return root.resolve(relative);
    }

    /** Finds a copybook by member name (extension and case insensitive), e.g. {@code CVACT01Y}. */
    public Path copybook(String member) {
        String wanted = member.toUpperCase(Locale.ROOT);
        for (String dir : COPYBOOK_DIRS) {
            Path d = root.resolve(dir);
            if (!Files.isDirectory(d)) {
                continue;
            }
            try (Stream<Path> files = Files.list(d)) {
                Optional<Path> hit = files.filter(f -> baseName(f).equals(wanted)).findFirst();
                if (hit.isPresent()) {
                    return hit.get();
                }
            } catch (IOException e) {
                throw new UncheckedIOException(e);
            }
        }
        throw new IllegalArgumentException("Copybook " + member + " not found under " + COPYBOOK_DIRS);
    }

    public Copybook parseCopybook(String member) {
        return CopybookParser.parse(copybook(member));
    }

    /** Layout of the first 01-level record of a copybook. */
    public RecordLayout layout(String member) {
        return parseCopybook(member).first();
    }

    public Path ebcdicData(String datasetName) {
        return existing(root.resolve("app/data/EBCDIC").resolve(datasetName));
    }

    public Path asciiData(String fileName) {
        return existing(root.resolve("app/data/ASCII").resolve(fileName));
    }

    private static Path existing(Path p) {
        if (!Files.isRegularFile(p)) {
            throw new IllegalArgumentException("Sample dataset not found: " + p);
        }
        return p;
    }

    private static String baseName(Path f) {
        String n = f.getFileName().toString().toUpperCase(Locale.ROOT);
        return n.contains(".") ? n.substring(0, n.lastIndexOf('.')) : n;
    }
}
