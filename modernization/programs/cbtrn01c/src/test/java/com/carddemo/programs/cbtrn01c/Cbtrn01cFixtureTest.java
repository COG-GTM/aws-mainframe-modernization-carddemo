package com.carddemo.programs.cbtrn01c;

import com.carddemo.programs.cbtrn01c.copybook.Pic;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

/** Runs the port end to end on the repository's app/data/ASCII datasets through fixed-width files. */
class Cbtrn01cFixtureTest {

    @TempDir
    Path tmp;

    private Map<String, String> dd;

    @BeforeEach
    void bindDds() throws Exception {
        Path data = SampleData.dir();
        assumeTrue(data != null, "sample data not available");
        dd = new HashMap<>(Map.of(
                "DALYTRAN", data.resolve("dailytran.txt").toString(),
                "CUSTFILE", data.resolve("custdata.txt").toString(),
                "XREFFILE", data.resolve("cardxref.txt").toString(),
                "CARDFILE", data.resolve("carddata.txt").toString(),
                "ACCTFILE", data.resolve("acctdata.txt").toString(),
                // TRANFILE.jcl seeds the transaction master from the daily transaction extract.
                "TRANFILE", data.resolve("dailytran.txt").toString()));
    }

    @Test
    void sampleDailyTransactionsAllVerify() throws Exception {
        List<String> out = new ArrayList<>();

        assertEquals(0, Cbtrn01c.execute(dd, out::add));

        List<String> daily = Files.readAllLines(Path.of(dd.get("DALYTRAN")), StandardCharsets.ISO_8859_1);
        assertEquals(300, daily.size());
        assertEquals("START OF EXECUTION OF PROGRAM CBTRN01C", out.get(0));
        assertEquals("END OF EXECUTION OF PROGRAM CBTRN01C", out.get(out.size() - 1));
        // 300 records + the EOF re-lookup of the last record
        assertEquals(301, count(out, "SUCCESSFUL READ OF XREF"));
        assertEquals(301, count(out, "SUCCESSFUL READ OF ACCOUNT FILE"));
        assertEquals(0, out.stream().filter(l -> l.contains("COULD NOT BE VERIFIED")).count());
        assertEquals(0, out.stream().filter(l -> l.endsWith(" NOT FOUND")).count());
        // 1 start + 300 x (record + 4 xref lines + 1 account line) + 5 EOF pass lines + 1 end
        assertEquals(1 + 300 * 6 + 5 + 1, out.size());
        assertEquals(Pic.alpha(daily.get(0), 350), out.get(1));
        assertEquals(List.of("SUCCESSFUL READ OF XREF", "CARD NUMBER: 4859452612877065",
                "ACCOUNT ID : 00000000007", "CUSTOMER ID: 000000007", "SUCCESSFUL READ OF ACCOUNT FILE"),
                out.subList(2, 7));
    }

    @Test
    void missingAccountFileAbendsWithStatus35() {
        dd.put("ACCTFILE", tmp.resolve("missing.txt").toString());
        List<String> out = new ArrayList<>();

        assertEquals(999, Cbtrn01c.execute(dd, out::add));

        assertEquals(List.of("ERROR OPENING ACCOUNT FILE", "FILE STATUS IS: NNNN0035", "ABENDING PROGRAM"),
                out.subList(out.size() - 3, out.size()));
    }

    @Test
    void overlongDailyRecordAbendsWithStatus04() throws Exception {
        Path bad = Files.writeString(tmp.resolve("bad.txt"), "X".repeat(351) + "\n");
        dd.put("DALYTRAN", bad.toString());
        List<String> out = new ArrayList<>();

        assertEquals(999, Cbtrn01c.execute(dd, out::add));

        assertEquals(List.of("ERROR READING DAILY TRANSACTION FILE", "FILE STATUS IS: NNNN0004",
                "ABENDING PROGRAM"), out.subList(out.size() - 3, out.size()));
    }

    private static long count(List<String> out, String line) {
        return out.stream().filter(line::equals).count();
    }
}
