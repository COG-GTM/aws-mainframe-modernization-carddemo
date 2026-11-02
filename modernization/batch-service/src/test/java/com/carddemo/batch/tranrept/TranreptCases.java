package com.carddemo.batch.tranrept;

import com.carddemo.common.batch.DdFiles;
import com.carddemo.common.record.CobolRecord;
import com.carddemo.common.record.CodePage;
import com.carddemo.common.record.layout.TranRecordLayout;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

/**
 * The TRANREPT cases under {@code src/test/resources/tranrept}: inputs plus the expected TRANREPT and
 * RETURN-CODE written by the GnuCOBOL reference ({@code scripts/migration/tranrept/reference.sh --write}).
 */
record TranreptCases(Path dir) {

    static final Path ROOT = Path.of("src/test/resources/tranrept");
    static final Path ASCII_DATA = Path.of("..", "..", "app", "data", "ASCII");

    static Stream<TranreptCases> all() throws IOException {
        try (Stream<Path> dirs = Files.list(ROOT)) {
            return dirs.filter(Files::isDirectory).sorted().map(TranreptCases::new).toList().stream();
        }
    }

    String name() {
        return dir.getFileName().toString();
    }

    boolean programMode() {
        return read("MODE").strip().equals("program");
    }

    int expectedReturnCode() {
        return Integer.parseInt(read("expected/RETURN-CODE").strip());
    }

    Optional<byte[]> expectedReport() {
        Path file = dir.resolve("expected/TRANREPT");
        try {
            return Files.isRegularFile(file) ? Optional.of(Files.readAllBytes(file)) : Optional.empty();
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    Path transact() {
        return dir.resolve("TRANSACT.txt");
    }

    Path dateparm() {
        return dir.resolve("DATEPARM.txt");
    }

    /** {@code CARDXREF.txt} etc. of the case, else the app/data/ASCII extract. */
    Path reference(String dd) {
        Path own = dir.resolve(dd + ".txt");
        return Files.isRegularFile(own) ? own : ASCII_DATA.resolve(dd.toLowerCase(java.util.Locale.ROOT) + ".txt");
    }

    List<CobolRecord> transactions() {
        return DdFiles.read(transact(), TranRecordLayout.LAYOUT, CodePage.ASCII);
    }

    Optional<DateParm> dateParmRecord() {
        return DdFiles.read(dateparm(), DateParm.LAYOUT, CodePage.ASCII).stream().findFirst().map(DateParm::of);
    }

    private String read(String name) {
        try {
            return Files.readString(dir.resolve(name));
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    @Override
    public String toString() {
        return name();
    }
}
