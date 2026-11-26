package com.carddemo.programs.cbtrn03c;

import com.carddemo.programs.cbtrn03c.io.FixedWidthRecords;
import com.carddemo.programs.cbtrn03c.io.RecordFormat;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

/** Access to src/test/resources/fixtures (copies of app/data/ASCII plus GnuCOBOL reference output). */
public final class Fixtures {
    private Fixtures() {
    }

    public static Path path(String name) {
        try {
            return Path.of(Fixtures.class.getResource("/fixtures/" + name).toURI());
        } catch (URISyntaxException e) {
            throw new IllegalStateException(e);
        }
    }

    public static List<String> lines(String name, int lrecl) throws IOException {
        return FixedWidthRecords.load(path(name), lrecl, RecordFormat.LINE);
    }

    /**
     * TRANREPT STEP05R input preparation for the ASCII sample: dailytran.txt has no TRAN-PROC-TS (that is
     * stamped by POSTTRAN/CBTRN02C), so TRAN-ORIG-TS is copied into TRAN-PROC-TS; then the SORT step's
     * INCLUDE (TRAN-PROC-DT within the parm dates) and SORT FIELDS=(TRAN-CARD-NUM,A) are applied (stable).
     */
    public static List<String> dailyTransactionsForReport(String startDate, String endDate) throws IOException {
        return lines("dailytran.txt", 350).stream()
                .map(r -> r.substring(0, 304) + r.substring(278, 304) + r.substring(330))
                .filter(r -> r.substring(304, 314).compareTo(startDate) >= 0
                        && r.substring(304, 314).compareTo(endDate) <= 0)
                .sorted(Comparator.comparing(r -> r.substring(262, 278)))
                .collect(Collectors.toList());
    }
}
