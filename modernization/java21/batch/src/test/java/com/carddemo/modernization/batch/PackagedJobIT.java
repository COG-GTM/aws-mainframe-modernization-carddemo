package com.carddemo.modernization.batch;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

/** Runs the repackaged jar as its own process on each golden set and checks every output file and the exit code. */
class PackagedJobIT {
    @TempDir Path work;

    @ParameterizedTest
    @ValueSource(strings = {"mixed", "volume"})
    void packagedJarReproducesGoldenSet(String set) throws Exception {
        Path jar = Path.of(System.getProperty("batch.jar"));
        assertTrue(Files.isRegularFile(jar), "packaged jar missing: " + jar);
        GoldenSet golden = new GoldenSet(GoldenSet.defaultGolden().toAbsolutePath(), set);
        Path datasets = work.resolve("datasets");
        Path runRoot = work.resolve("runs");
        golden.stage(datasets);

        List<String> command = new ArrayList<>(List.of(
                Path.of(System.getProperty("java.home"), "bin", "java").toString(), "-jar", jar.toString()));
        command.addAll(golden.arguments(datasets, runRoot, set));
        Process process = new ProcessBuilder(command).directory(work.toFile()).redirectErrorStream(true).start();
        String output = new String(process.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
        assertTrue(process.waitFor(5, TimeUnit.MINUTES), "jar did not finish");
        int exitCode = process.exitValue();
        System.out.print(output);

        assertEquals(golden.expectedExitCode(), exitCode, "process exit code\n" + output);
        Path runDir = runRoot.resolve(set);
        try (Stream<Path> files = Files.list(runDir)) {
            assertEquals(Set.copyOf(GoldenSet.RUN_OUTPUTS),
                    files.map(f -> f.getFileName().toString()).collect(Collectors.toSet()), "run directory contents");
        }
        List<String> summary = golden.assertIdentical(golden.comparisons(datasets, runDir));
        System.out.println("E2E " + set + ": exit code " + exitCode + " (expected " + golden.expectedExitCode()
                + "), " + summary.size() + " files byte-identical to golden/expected/" + set);
        summary.forEach(line -> System.out.println("  " + line));
    }
}
