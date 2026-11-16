package com.carddemo.batch.jobs.creastmt;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.List;

/**
 * CREASTMT STEP010 (SORT): {@code SORT FIELDS=(263,16,CH,A,1,16,CH,A)} and
 * {@code OUTREC FIELDS=(1:263,16,17:1,262,279:279,50)} turn TRANSACT records into the
 * 350-byte TRXFL layout (COSTM01) keyed by card number + transaction id.
 */
public final class TrxflSorter {

    private TrxflSorter() {
    }

    public static void sort(Path transactFile, Path trxflFile) throws IOException {
        List<String> trxfl = Files.readAllLines(transactFile, StandardCharsets.US_ASCII).stream()
                .filter(line -> !line.isEmpty())
                .map(line -> Cobol.pic(line, 350))
                .sorted(Comparator.<String, String>comparing(r -> r.substring(262, 278))
                        .thenComparing(r -> r.substring(0, 16)))
                .map(r -> Cobol.pic(r.substring(262, 278) + r.substring(0, 262) + r.substring(278, 328), 350))
                .toList();
        Files.createDirectories(trxflFile.toAbsolutePath().getParent());
        Files.write(trxflFile, trxfl, StandardCharsets.US_ASCII);
    }
}
