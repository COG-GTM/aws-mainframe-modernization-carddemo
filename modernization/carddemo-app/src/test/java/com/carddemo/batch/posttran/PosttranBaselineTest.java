package com.carddemo.batch.posttran;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.account.AccountRecord;
import com.carddemo.batch.harness.FixedFileSink;
import com.carddemo.batch.harness.KeyedDataset;
import com.carddemo.batch.harness.KsdsInput;
import com.carddemo.batch.harness.ReturnCode;
import com.carddemo.batch.harness.Sysout;
import com.carddemo.batch.print.PrintProgramsBaselineTest;
import com.carddemo.batch.print.ProgramCounts;
import com.carddemo.card.CardRecord;
import com.carddemo.card.CardXrefRecord;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.codec.TestData;
import com.carddemo.customer.CustomerRecord;
import com.carddemo.transaction.DailyTransactionRecord;
import com.carddemo.transaction.TranCatBalanceId;
import com.carddemo.transaction.TranCatBalanceRecord;
import com.carddemo.transaction.TransactionRecord;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.stream.Collectors;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * POSTTRAN STEP10 + STEP15 on the inputs the GnuCOBOL baseline used ({@code app/data/ASCII}; TRANFILE starts empty:
 * the baseline's LOW-VALUES seed record is never read and is replaced when CBTRN02C opens TRANFILE OUTPUT), compared with {@code docs/validation/baseline/CBTRN01C} and
 * {@code POSTTRAN}: SYSOUT (without the baseline runner's IDCAMS-EMU after-image unload lines), RC, DALYREJS, and the
 * TRANSACT/ACCTDATA/TCATBALF after-images byte for byte.
 */
class PosttranBaselineTest {

    static final Clock GOLDEN = Clock.fixed(Instant.parse("2022-07-06T00:00:00Z"), ZoneOffset.UTC);

    @TempDir
    Path dir;

    private Path copy(String from, String to) throws IOException {
        Path target = dir.resolve(to);
        Files.copy(TestData.resolve(from), target, StandardCopyOption.REPLACE_EXISTING);
        return target;
    }

    private static KsdsInput ascii(String dd, String sample, com.carddemo.common.codec.RecordLayout layout) {
        return KsdsInput.file(dd, TestData.resolve("app/data/ASCII/" + sample), layout, RecordEncoding.ASCII);
    }

    private static List<String> lines(Path path) throws IOException {
        return Files.readAllLines(path, StandardCharsets.ISO_8859_1);
    }

    @Test
    void posttranMatchesTheBaseline() throws IOException {
        Path acct = copy("app/data/ASCII/acctdata.txt", "ACCTDATA.ksds");
        Path tcatbal = copy("app/data/ASCII/tcatbal.txt", "TCATBALF.ksds");
        Path tran = Files.createFile(dir.resolve("TRANSACT.ksds"));
        Path xrefFile = TestData.resolve("app/data/ASCII/cardxref.txt");

        Path sysout01 = dir.resolve("cbtrn01c.txt");
        ProgramCounts counts;
        try (Sysout sysout = Sysout.open(sysout01)) {
            counts = new Cbtrn01c(ascii("DALYTRAN", "dailytran.txt", DailyTransactionRecord.MAPPER.layout()),
                    ascii("CUSTFILE", "custdata.txt", CustomerRecord.MAPPER.layout()),
                    xref(xrefFile), ascii("CARDFILE", "carddata.txt", CardRecord.MAPPER.layout()),
                    account(acct, KeyedDataset.Mode.INPUT),
                    KsdsInput.file("TRANFILE", tran, TransactionRecord.MAPPER.layout(), RecordEncoding.ASCII),
                    sysout).run();
        }
        assertThat(counts.read()).isEqualTo(300);
        assertThat(PrintProgramsBaselineTest.sysoutLines(sysout01))
                .containsExactlyElementsOf(PrintProgramsBaselineTest.baselineSysout("CBTRN01C"));

        Path sysout02 = dir.resolve("posttran.txt");
        Path rejects = dir.resolve("DALYREJS");
        Cbtrn02c.Result result;
        try (Sysout sysout = Sysout.open(sysout02)) {
            result = new Cbtrn02c(ascii("DALYTRAN", "dailytran.txt", DailyTransactionRecord.MAPPER.layout()),
                    KeyedDataset.file("TRANFILE", tran, KeyedDataset.Mode.OUTPUT, TransactionRecord.MAPPER,
                            TransactionRecord::tranId, k -> PosttranJobConfiguration.pad(k, 16),
                            RecordEncoding.ASCII),
                    xref(xrefFile), new FixedFileSink("DALYREJS", rejects), account(acct, KeyedDataset.Mode.I_O),
                    KeyedDataset.file("TCATBALF", tcatbal, KeyedDataset.Mode.I_O, TranCatBalanceRecord.MAPPER,
                            r -> new TranCatBalanceId(r.acctId(), r.tranTypeCd(), r.tranCatCd()),
                            Cbtrn02c::tcatKey, RecordEncoding.ASCII),
                    sysout, GOLDEN, Cbtrn02c.UnitOfWork.NONE).run();
        }
        assertThat(result).isEqualTo(new Cbtrn02c.Result(300, 38, ReturnCode.WARNING));
        assertThat(lines(TestData.resolve("docs/validation/baseline/POSTTRAN/rc.txt")).get(0).trim())
                .isEqualTo(String.valueOf(result.returnCode().code()));
        assertThat(PrintProgramsBaselineTest.sysoutLines(sysout02))
                .containsExactlyElementsOf(PrintProgramsBaselineTest.baselineSysout("POSTTRAN").stream()
                        .filter(l -> !l.startsWith("--- IDCAMS-EMU ") && !l.startsWith("IDCAMS-EMU "))
                        .toList());

        List<String> baselineRejects = PrintProgramsBaselineTest.baselineFile("POSTTRAN", "DALYREJS");
        assertThat(PrintProgramsBaselineTest.fold(rejects, Cbtrn02c.REJECT_LRECL))
                .containsExactlyElementsOf(baselineRejects);
        Map<String, Long> reasons = baselineRejects.stream()
                .collect(Collectors.groupingBy(l -> l.substring(350, 354), TreeMap::new, Collectors.counting()));
        assertThat(reasons).containsExactly(Map.entry("0102", 38L));

        for (String ksds : List.of("TRANSACT", "ACCTDATA", "TCATBALF")) {
            assertThat(lines(dir.resolve(ksds + ".ksds"))).as(ksds)
                    .containsExactlyElementsOf(lines(TestData.resolve(
                            "docs/validation/baseline/POSTTRAN/" + ksds + ".ksds.txt")));
        }
    }

    private static KeyedDataset<String, CardXrefRecord> xref(Path file) {
        return KeyedDataset.file("XREFFILE", file, KeyedDataset.Mode.INPUT, CardXrefRecord.MAPPER,
                CardXrefRecord::cardNum, k -> PosttranJobConfiguration.pad(k, 16), RecordEncoding.ASCII);
    }

    private static KeyedDataset<Long, AccountRecord> account(Path file, KeyedDataset.Mode mode) {
        return KeyedDataset.file("ACCTFILE", file, mode, AccountRecord.MAPPER, AccountRecord::acctId,
                k -> String.format("%011d", k), RecordEncoding.ASCII);
    }
}
