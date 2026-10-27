package com.carddemo.modernization.batch;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.carddemo.modernization.records.CardXrefRecord;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
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
        // A non-empty directory where b's temporary file would go makes b's staging fail after a's has succeeded.
        Path blocker = second.resolveSibling("XREFFILE.bin.tmp");
        Files.createDirectory(blocker);
        Files.writeString(blocker.resolve("keep"), "x");

        assertThrows(RuntimeException.class, () -> KeyedDataset.flush(a, b));
        assertArrayEquals(Files.readAllBytes(golden), Files.readAllBytes(first), "a not replaced");
        assertArrayEquals(Files.readAllBytes(golden), Files.readAllBytes(second), "b not replaced");
        try (Stream<Path> files = Files.list(first.getParent())) {
            assertEquals(List.of(first), files.toList(), "a's staged file removed");
        }
    }
}
