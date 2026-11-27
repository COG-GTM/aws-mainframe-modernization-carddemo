package com.carddemo.data.report;

import com.carddemo.common.cobol.DataEncoding;
import com.carddemo.common.codec.RecordCodec;
import com.carddemo.common.copybook.RecordLayout;
import com.carddemo.common.io.RecordFormat;
import com.carddemo.common.testing.CardDemoDataset;
import com.carddemo.common.testing.ComparisonReport;
import com.carddemo.common.testing.GoldenMaster;
import com.carddemo.common.testing.MainframeSources;
import com.carddemo.data.catalog.CardDemoFile;
import com.carddemo.data.load.CardDemoDataImporter;
import com.carddemo.data.testing.CardDemoDataTest;
import com.carddemo.data.testing.CardDemoFiles;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.repository.JpaRepository;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Runs the parity and performance checks over every sample dataset and writes a self-contained HTML report to
 * {@code target/carddemo-test-report/index.html}. Run alone with {@code mvn verify -pl carddemo-data -am -Dgroups=report}.
 */
@CardDemoDataTest
@Tag("report")
class MigrationTestReportTest {

    private static final int PERF_ITERATIONS = 200;

    record Row(CardDemoDataset dataset, int records, int fields, int codecDiffs, int dbDiffs, long loadMs,
               long unloadMs, double decodePerSec, double encodePerSec) {
    }

    @Autowired
    CardDemoDataImporter importer;

    @Autowired
    List<JpaRepository<?, ?>> repositories;

    @Test
    void writeParityAndPerformanceReport() throws Exception {
        MainframeSources sources = MainframeSources.locate();
        GoldenMaster golden = new GoldenMaster(Paths.get("target"));
        repositories.forEach(JpaRepository::deleteAllInBatch);
        List<Row> rows = new ArrayList<>();
        for (CardDemoDataset dataset : CardDemoDataset.values()) {
            CardDemoFile file = CardDemoFiles.of(dataset);
            RecordLayout layout = file.layout();
            RecordCodec<Record> codec = file.codec(DataEncoding.EBCDIC);
            List<byte[]> original = dataset.ebcdicRecords(sources);

            List<Record> decoded = new ArrayList<>();
            List<byte[]> reencoded = new ArrayList<>();
            original.forEach(r -> decoded.add(codec.decode(r)));
            decoded.forEach(r -> reencoded.add(codec.encode(r)));
            ComparisonReport codecParity = golden.compare(dataset + " codec", layout, original, DataEncoding.EBCDIC,
                    reencoded, DataEncoding.EBCDIC, false);

            long t0 = System.nanoTime();
            importer.load(file, sources.ebcdicData(dataset.ebcdicFile()), RecordFormat.FIXED, DataEncoding.EBCDIC);
            long t1 = System.nanoTime();
            List<byte[]> unloaded = importer.unload(file, DataEncoding.EBCDIC);
            long t2 = System.nanoTime();
            ComparisonReport dbParity = golden.compare(dataset + " db", layout, original, DataEncoding.EBCDIC,
                    unloaded, DataEncoding.EBCDIC, false);

            long d0 = System.nanoTime();
            for (int i = 0; i < PERF_ITERATIONS; i++) {
                for (byte[] r : original) {
                    codec.decode(r);
                }
            }
            long d1 = System.nanoTime();
            for (int i = 0; i < PERF_ITERATIONS; i++) {
                for (Record r : decoded) {
                    codec.encode(r);
                }
            }
            long d2 = System.nanoTime();
            double ops = (double) PERF_ITERATIONS * original.size();
            rows.add(new Row(dataset, original.size(), layout.primaryElementaryFields().size(),
                    codecParity.differences().size() + Math.abs(codecParity.expectedCount() - codecParity.actualCount()),
                    dbParity.differences().size() + Math.abs(dbParity.expectedCount() - dbParity.actualCount()),
                    (t1 - t0) / 1_000_000, (t2 - t1) / 1_000_000, ops / ((d1 - d0) / 1e9), ops / ((d2 - d1) / 1e9)));
        }
        Path out = Paths.get("target", "carddemo-test-report", "index.html");
        Files.createDirectories(out.getParent());
        Files.writeString(out, html(rows), StandardCharsets.UTF_8);
        assertThat(rows).allSatisfy(r -> {
            assertThat(r.codecDiffs()).isZero();
            assertThat(r.dbDiffs()).isZero();
        });
    }

    private static String html(List<Row> rows) {
        int records = rows.stream().mapToInt(Row::records).sum();
        long values = rows.stream().mapToLong(r -> (long) r.records() * r.fields()).sum();
        int diffs = rows.stream().mapToInt(r -> r.codecDiffs() + r.dbDiffs()).sum();
        double maxRate = rows.stream().mapToDouble(r -> Math.max(r.decodePerSec(), r.encodePerSec())).max().orElse(1);
        long maxMs = Math.max(1, rows.stream().mapToLong(r -> Math.max(r.loadMs(), r.unloadMs())).max().orElse(1));
        StringBuilder parity = new StringBuilder();
        StringBuilder perf = new StringBuilder();
        StringBuilder db = new StringBuilder();
        for (Row r : rows) {
            parity.append(String.format("<tr><td><b>%s</b><br><small>%s · LRECL %d</small></td><td>%d</td><td>%d</td>"
                            + "<td>%,d</td><td class=\"%s\">%s</td><td class=\"%s\">%s</td></tr>",
                    r.dataset(), r.dataset().copybook(), r.dataset().lrecl(), r.records(), r.fields(),
                    (long) r.records() * r.fields(), r.codecDiffs() == 0 ? "ok" : "ko",
                    r.codecDiffs() == 0 ? "&#10004; 100%" : r.codecDiffs() + " diffs",
                    r.dbDiffs() == 0 ? "ok" : "ko", r.dbDiffs() == 0 ? "&#10004; 100%" : r.dbDiffs() + " diffs"));
            perf.append(bar(r.dataset().name(), r.decodePerSec(), maxRate, "#2563eb", "%,.0f rec/s decode"))
                    .append(bar("", r.encodePerSec(), maxRate, "#7c3aed", "%,.0f rec/s encode"));
            db.append(bar(r.dataset().name(), r.loadMs(), maxMs, "#059669", "%.0f ms load"))
                    .append(bar("", r.unloadMs(), maxMs, "#d97706", "%.0f ms unload"));
        }
        return """
                <!doctype html><html lang="en"><head><meta charset="utf-8"><title>CardDemo Java 21 - parity &amp; performance</title>
                <style>
                body{font-family:system-ui,-apple-system,Segoe UI,sans-serif;margin:0;background:#f5f7fb;color:#1f2937}
                header{background:linear-gradient(120deg,#0f172a,#1e3a8a);color:#fff;padding:28px 40px}
                header h1{margin:0 0 6px;font-size:26px} header p{margin:0;opacity:.8}
                main{padding:24px 40px;max-width:1150px}
                .cards{display:flex;gap:16px;margin-bottom:24px;flex-wrap:wrap}
                .card{background:#fff;border-radius:12px;padding:18px 22px;box-shadow:0 1px 3px rgba(0,0,0,.08);min-width:190px}
                .card .v{font-size:30px;font-weight:700} .card .l{color:#6b7280;font-size:13px}
                section{background:#fff;border-radius:12px;padding:20px 24px;margin-bottom:24px;box-shadow:0 1px 3px rgba(0,0,0,.08)}
                h2{margin-top:0;font-size:18px} table{border-collapse:collapse;width:100%%} td,th{padding:8px 10px;border-bottom:1px solid #eef0f4;text-align:left;font-size:14px}
                th{color:#6b7280;font-weight:600} .ok{color:#059669;font-weight:700} .ko{color:#dc2626;font-weight:700}
                .bar{display:flex;align-items:center;gap:10px;margin:3px 0;font-size:13px} .bar .n{width:110px;font-weight:600}
                .bar .t{flex:1;background:#eef0f4;border-radius:6px;height:16px} .bar .f{height:16px;border-radius:6px} .bar .x{width:170px;color:#374151}
                small{color:#6b7280} .note{color:#6b7280;font-size:13px}
                </style></head><body>
                <header><h1>CardDemo COBOL &rarr; Java 21 &middot; Parity &amp; performance report</h1>
                <p>Generated %s by MigrationTestReportTest &middot; sample datasets from app/data/EBCDIC &middot; PostgreSQL 16 (Testcontainers)</p></header>
                <main>
                <div class="cards">
                <div class="card"><div class="v">%d</div><div class="l">datasets checked</div></div>
                <div class="card"><div class="v">%,d</div><div class="l">mainframe records</div></div>
                <div class="card"><div class="v">%,d</div><div class="l">field values compared (x2 paths)</div></div>
                <div class="card"><div class="v %s">%d</div><div class="l">parity differences</div></div>
                </div>
                <section><h2>Parity (golden master, field by field vs EBCDIC dataset)</h2>
                <table><tr><th>Dataset</th><th>Records</th><th>Fields</th><th>Values</th><th>Copybook codec round trip</th><th>Load &rarr; PostgreSQL &rarr; unload</th></tr>%s</table>
                <p class="note">Each record is decoded with its copybook, re-encoded, and compared field by field; the database path maps
                records to JPA entities, stores them in PostgreSQL, reads them back and re-encodes them in VSAM key order. FILLER is not persisted.</p></section>
                <section><h2>Codec throughput (%d iterations per dataset, single thread)</h2>%s</section>
                <section><h2>Database load / unload time (Flyway schema, Hibernate, PostgreSQL)</h2>%s</section>
                </main></body></html>
                """.formatted(LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")), rows.size(),
                records, values, diffs == 0 ? "ok" : "ko", diffs, parity, PERF_ITERATIONS, perf, db);
    }

    private static String bar(String name, double value, double max, String color, String label) {
        double pct = Math.max(1, value / max * 100);
        return String.format("<div class=\"bar\"><span class=\"n\">%s</span><span class=\"t\"><div class=\"f\" "
                + "style=\"width:%.1f%%;background:%s\"></div></span><span class=\"x\">%s</span></div>",
                name, pct, color, String.format(label, value));
    }
}
