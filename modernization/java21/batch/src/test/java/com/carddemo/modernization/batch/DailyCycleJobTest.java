package com.carddemo.modernization.batch;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.batch.core.BatchStatus;
import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.JobExecution;
import org.springframework.boot.SpringApplication;

/** In-process runs of the job; {@link PackagedJobIT} covers the packaged jar. */
class DailyCycleJobTest {
    @TempDir Path work;

    private final GoldenSet mixed = new GoldenSet(GoldenSet.defaultGolden(), "mixed");

    @ParameterizedTest
    @ValueSource(ints = {1, 3, 100})
    void outputsDoNotDependOnChunkSize(int chunkSize) throws Exception {
        Path datasets = work.resolve("datasets");
        mixed.stage(datasets);
        assertEquals(4, run(mixed.arguments(datasets, work.resolve("runs"), "mixed"),
                "--carddemo.posting.chunk-size=" + chunkSize));
        mixed.assertIdentical(mixed.comparisons(datasets, work.resolve("runs/mixed")));
    }

    @Test
    void reusedRunIdFailsBeforeAnyStepAndLeavesDatasetsAlone() throws Exception {
        Path datasets = work.resolve("datasets");
        mixed.stage(datasets);
        Files.createDirectories(work.resolve("runs/mixed"));
        assertEquals(CycleExitCodeGenerator.ABEND, run(mixed.arguments(datasets, work.resolve("runs"), "mixed")));
        for (String dd : GoldenSet.MASTERS) {
            assertArrayEquals(Files.readAllBytes(mixed.expected().resolve("initial." + dd + ".bin")),
                    Files.readAllBytes(datasets.resolve(dd + ".bin")), dd);
        }
        try (var files = Files.list(work.resolve("runs/mixed"))) {
            assertEquals(0, files.count());
        }
    }

    @Test
    void interestAbendAfterPostingRejectsGivesAbendCode() throws Exception {
        Path datasets = work.resolve("datasets");
        mixed.stage(datasets);
        Files.write(datasets.resolve("DISCGRP.bin"), new byte[0]);
        assertEquals(CycleExitCodeGenerator.ABEND, run(mixed.arguments(datasets, work.resolve("runs"), "mixed")));

        Path runDir = work.resolve("runs/mixed");
        assertArrayEquals(Files.readAllBytes(mixed.expected().resolve("DALYREJS.bin")),
                Files.readAllBytes(runDir.resolve("DALYREJS.bin")));
        assertArrayEquals(Files.readAllBytes(mixed.expected().resolve("CBTRN02C.ACCTFILE.bin")),
                Files.readAllBytes(datasets.resolve("ACCTFILE.bin")), "post-posting ACCTFILE is left as posted");
        assertFalse(Files.exists(runDir.resolve("SYSTRAN.bin")));
        List<String> sysout = Files.readAllLines(runDir.resolve("CBACT04C.sysout.txt"));
        assertEquals("ABENDING PROGRAM", sysout.get(sysout.size() - 1));
    }

    @Test
    void parmDateMustBeTenCharacters() throws Exception {
        Path datasets = work.resolve("datasets");
        mixed.stage(datasets);
        List<String> args = new ArrayList<>(mixed.arguments(datasets, work.resolve("runs"), "mixed"));
        args.replaceAll(a -> a.startsWith("parmDate=") ? "parmDate=20220718" : a);
        assertThrows(IllegalStateException.class, () -> run(args));
        assertFalse(Files.exists(work.resolve("runs/mixed")));
    }

    @Test
    void exitCodeIsHighestStepReturnCode() {
        assertEquals(0, CycleExitCodeGenerator.exitCode(job(BatchStatus.COMPLETED, ExitStatus.COMPLETED, ExitStatus.COMPLETED)));
        assertEquals(4, CycleExitCodeGenerator.exitCode(
                job(BatchStatus.COMPLETED, PostingStepWriter.COMPLETED_WITH_REJECTS, ExitStatus.COMPLETED)));
        assertEquals(12, CycleExitCodeGenerator.exitCode(
                job(BatchStatus.COMPLETED, PostingStepWriter.COMPLETED_WITH_REJECTS, ExitStatus.FAILED)));
        assertEquals(12, CycleExitCodeGenerator.exitCode(job(BatchStatus.FAILED, ExitStatus.COMPLETED)));
    }

    private static JobExecution job(BatchStatus status, ExitStatus... steps) {
        JobExecution job = new JobExecution(1L);
        job.setStatus(status);
        for (int i = 0; i < steps.length; i++) {
            job.createStepExecution("step" + i).setExitStatus(steps[i]);
        }
        return job;
    }

    private static int run(List<String> args, String... extra) {
        List<String> all = new ArrayList<>(args);
        all.addAll(List.of(extra));
        return SpringApplication.exit(SpringApplication.run(CardDemoBatchApplication.class, all.toArray(String[]::new)));
    }
}
