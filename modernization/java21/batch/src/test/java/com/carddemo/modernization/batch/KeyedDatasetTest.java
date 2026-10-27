package com.carddemo.modernization.batch;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.carddemo.modernization.records.CardXrefRecord;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.PosixFilePermission;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.List;
import java.util.Set;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class KeyedDatasetTest {
    @TempDir Path work;

    @Test
    void failedStagingReplacesNoMasterAndLeavesNoTemporaryFile() throws Exception {
        Path golden = GoldenSet.defaultGolden().resolve("expected/mixed/initial.XREFFILE.bin");
        Path first = work.resolve("a/XREFFILE.bin");
        Path second = work.resolve("b/XREFFILE.bin");
        Files.createDirectories(first.getParent());
        Files.createDirectories(second.getParent());
        Files.copy(golden, first);
        Files.copy(golden, second);
        KeyedDataset<CardXrefRecord> a = KeyedDataset.load(first, CardXrefRecord.CODEC, 16, StandardCharsets.US_ASCII);
        KeyedDataset<CardXrefRecord> b = KeyedDataset.load(second, CardXrefRecord.CODEC, 16, StandardCharsets.US_ASCII);
        a.clear();
        b.clear();
        // An existing path that looks like a temporary file is not the job's and must survive.
        Path unrelated = Files.createDirectory(first.resolveSibling("XREFFILE.bin.tmp"));
        // A read-only directory makes b's staging fail after a's has succeeded.
        Set<PosixFilePermission> writable = Files.getPosixFilePermissions(second.getParent());
        Files.setPosixFilePermissions(second.getParent(), PosixFilePermissions.fromString("r-x------"));
        try {
            assertThrows(RuntimeException.class, () -> KeyedDataset.flush(a, b));
        } finally {
            Files.setPosixFilePermissions(second.getParent(), writable);
        }
        assertArrayEquals(Files.readAllBytes(golden), Files.readAllBytes(first), "a not replaced");
        assertArrayEquals(Files.readAllBytes(golden), Files.readAllBytes(second), "b not replaced");
        try (Stream<Path> files = Files.list(first.getParent())) {
            assertEquals(Set.of(first, unrelated), Set.copyOf(files.toList()), "only a's staged file removed");
        }
    }

    @Test
    void readOnlyMasterIsReplacedAndKeepsItsPermissions() throws Exception {
        Path master = work.resolve("XREFFILE.bin");
        Files.copy(GoldenSet.defaultGolden().resolve("expected/mixed/initial.XREFFILE.bin"), master);
        Set<PosixFilePermission> readOnly = PosixFilePermissions.fromString("r--r-----");
        Files.setPosixFilePermissions(master, readOnly);
        KeyedDataset<CardXrefRecord> dataset = KeyedDataset.load(master, CardXrefRecord.CODEC, 16, StandardCharsets.US_ASCII);
        dataset.clear();

        KeyedDataset.flush(dataset);
        assertEquals(0, Files.size(master));
        assertEquals(readOnly, Files.getPosixFilePermissions(master));
        try (Stream<Path> files = Files.list(work)) {
            assertEquals(List.of(master), files.toList(), "no temporary file left");
        }
    }
}
