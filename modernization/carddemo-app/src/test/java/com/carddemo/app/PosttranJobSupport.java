package com.carddemo.app;

import com.carddemo.app.data.KsdsDataService;
import com.carddemo.app.data.KsdsDataService.Ksds;
import com.carddemo.app.golden.GoldenCase;
import com.carddemo.app.golden.ParityHarness;
import com.carddemo.common.cobol.CobolCharsets;
import com.carddemo.common.record.RecordFiles;
import org.springframework.beans.factory.annotation.Autowired;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

/** Launches {@code POSTTRAN} through the real CLI seam ({@link CardDemoApplication#launch}) against the test container. */
abstract class PosttranJobSupport extends PostgresContainerSupport {

    static final DateTimeFormatter ORACLE_NOW = DateTimeFormatter.ofPattern("yyyy/MM/dd HH:mm:ss");
    /** A TRANSACT row left by a previous run; a new instance must remove it (D-024). */
    static final String STALE_TRAN_ID = "STALE00000000001";

    @Autowired
    KsdsDataService data;

    /** Masters exactly as the oracle loaded them for {@code c}, plus one stale TRANSACT row. */
    void seed(GoldenCase c) {
        data.deleteAll();
        for (Ksds ksds : List.of(Ksds.ACCTDATA, Ksds.CARDXREF, Ksds.TCATBALF)) {
            data.load(ksds, RecordFiles.readLines(c.master(ksds), ksds.recordLength(), CobolCharsets.ASCII));
        }
        String any = RecordFiles.readLines(GoldenCase.named("syn-ok").expected("TRANSACT.unl"), 350, CobolCharsets.ASCII).get(0);
        data.load(Ksds.TRANSACT, List.of(STALE_TRAN_ID + any.substring(16)));
    }

    /** {@code --job=POSTTRAN} with the case's DALYTRAN, clock pinned to the oracle's {@code COB_CURRENT_DATE}. */
    static int launch(GoldenCase c, Path dalytran, Path out, String runId, String... extra) {
        List<String> args = new ArrayList<>(List.of("--job=POSTTRAN", "--run-id=" + runId,
                "--dd=DALYTRAN=" + dalytran, "--dd=SYSOUT=" + out.resolve("SYSOUT"),
                "--carddemo.batch.clock=" + LocalDateTime.parse(c.oracleNow(), ORACLE_NOW),
                "--carddemo.posttran.dalytran-encoding=" + c.dalytranCharset().name()));
        args.addAll(List.of(extra));
        String[] all = Stream.concat(args.stream(), Stream.of(datasourceArgs())).toArray(String[]::new);
        return CardDemoApplication.launch(all, new PrintStream(new ByteArrayOutputStream(), true, StandardCharsets.UTF_8));
    }

    static int launch(GoldenCase c, Path out, String runId) {
        return launch(c, c.dalytran(), out, runId, "--dd=DALYREJS=" + out.resolve("DALYREJS"));
    }

    static List<String> sysout(Path out) {
        try {
            return Files.readAllLines(out.resolve("SYSOUT"), CobolCharsets.ASCII);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    /** DALYREJS bytes, the three tables POSTTRAN writes, SYSOUT and RC, all against the oracle. */
    void assertParity(GoldenCase c, Path out, int rc) {
        ParityHarness.assertReturnCode(c, rc);
        ParityHarness.assertDalyrejs(c, out.resolve("DALYREJS"));
        assertTables(c);
        ParityHarness.assertSysout(c, sysout(out));
    }

    void assertTables(GoldenCase c) {
        for (Ksds ksds : List.of(Ksds.ACCTDATA, Ksds.TCATBALF, Ksds.TRANSACT)) {
            ParityHarness.assertTable(c, ksds, data.unload(ksds));
        }
    }
}
