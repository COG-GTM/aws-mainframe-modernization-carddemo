package com.carddemo.batch.intcalc;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.account.AccountRecord;
import com.carddemo.batch.harness.FixedFileSink;
import com.carddemo.batch.harness.KeyedDataset;
import com.carddemo.batch.harness.KsdsInput;
import com.carddemo.batch.harness.ReturnCode;
import com.carddemo.batch.harness.Sysout;
import com.carddemo.batch.print.PrintProgramsBaselineTest;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.codec.TestData;
import com.carddemo.transaction.DisclosureGroupId;
import com.carddemo.transaction.DisclosureGroupRecord;
import com.carddemo.transaction.TranCatBalanceRecord;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * INTCALC STEP15 on the state the GnuCOBOL baseline ran it from ({@code scripts/baseline/baseline.py}: the POSTTRAN
 * after-images of ACCTDATA and TCATBALF, the {@code app/data/ASCII} CARDXREF and DISCGRP), PARM {@code 2022071800}
 * and the golden clock, compared with {@code docs/validation/baseline/INTCALC}: SYSOUT (without the runner's
 * {@code RUNCB04} PARM echo and IDCAMS-EMU unload lines), RC, SYSTRAN, and the ACCTDATA after-image byte for byte
 * (FILLER included); TCATBALF is opened INPUT and must still equal the POSTTRAN after-image.
 */
class IntcalcBaselineTest {

    static final Clock GOLDEN = Clock.fixed(Instant.parse("2022-07-06T00:00:00Z"), ZoneOffset.UTC);

    @TempDir
    Path dir;

    static List<String> baselineSysout() throws IOException {
        return PrintProgramsBaselineTest.baselineSysout("INTCALC").stream()
                .filter(l -> !l.startsWith("RUNCB04: ") && !l.startsWith("--- IDCAMS-EMU ")
                        && !l.startsWith("IDCAMS-EMU "))
                .toList();
    }

    private static List<String> lines(Path path) throws IOException {
        return Files.readAllLines(path, StandardCharsets.ISO_8859_1);
    }

    @Test
    void intcalcMatchesTheBaseline() throws IOException {
        Path acct = dir.resolve("ACCTDATA.ksds");
        Path tcatbal = dir.resolve("TCATBALF.ksds");
        Files.copy(TestData.resolve("docs/validation/baseline/POSTTRAN/ACCTDATA.ksds.txt"), acct,
                StandardCopyOption.REPLACE_EXISTING);
        Files.copy(TestData.resolve("docs/validation/baseline/POSTTRAN/TCATBALF.ksds.txt"), tcatbal,
                StandardCopyOption.REPLACE_EXISTING);
        Path systran = dir.resolve("SYSTRAN");
        Path sysoutPath = dir.resolve("sysout.txt");

        Cbact04c.Result result;
        try (Sysout sysout = Sysout.open(sysoutPath)) {
            result = new Cbact04c(
                    KsdsInput.file("TCATBALF", tcatbal, TranCatBalanceRecord.MAPPER.layout(), RecordEncoding.ASCII),
                    new XrefByAccount("XREFFILE", TestData.resolve("app/data/ASCII/cardxref.txt"),
                            RecordEncoding.ASCII),
                    KeyedDataset.file("DISCGRP", TestData.resolve("app/data/ASCII/discgrp.txt"),
                            KeyedDataset.Mode.INPUT, DisclosureGroupRecord.MAPPER,
                            r -> new DisclosureGroupId(r.acctGroupId(), r.tranTypeCd(), r.tranCatCd()),
                            IntcalcJobConfiguration::discgrpKey, RecordEncoding.ASCII),
                    KeyedDataset.file("ACCTFILE", acct, KeyedDataset.Mode.I_O, AccountRecord.MAPPER,
                            AccountRecord::acctId, k -> String.format("%011d", k), RecordEncoding.ASCII),
                    new FixedFileSink("TRANSACT", systran), "2022071800", RecordEncoding.ASCII, sysout,
                    GOLDEN).run();
        }

        assertThat(result).isEqualTo(new Cbact04c.Result(100, 50, 49, ReturnCode.OK));
        assertThat(lines(TestData.resolve("docs/validation/baseline/INTCALC/rc.txt")).get(0).trim())
                .isEqualTo(String.valueOf(result.returnCode().code()));
        assertThat(PrintProgramsBaselineTest.sysoutLines(sysoutPath)).containsExactlyElementsOf(baselineSysout());
        assertThat(PrintProgramsBaselineTest.fold(systran, Cbact04c.TRANSACT_LRECL))
                .containsExactlyElementsOf(PrintProgramsBaselineTest.baselineFile("INTCALC", "TRANSACT"));
        assertThat(lines(acct)).containsExactlyElementsOf(
                lines(TestData.resolve("docs/validation/baseline/INTCALC/ACCTDATA.ksds.txt")));
        assertThat(lines(tcatbal)).containsExactlyElementsOf(
                lines(TestData.resolve("docs/validation/baseline/POSTTRAN/TCATBALF.ksds.txt")));
    }
}
