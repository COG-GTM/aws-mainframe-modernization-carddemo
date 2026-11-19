package com.carddemo.app;

import com.carddemo.account.repository.AccountRepository;
import com.carddemo.app.golden.GoldenCase;
import com.carddemo.app.golden.GoldenCompare;
import com.carddemo.app.golden.TableUnloader;
import com.carddemo.card.domain.CardXref;
import com.carddemo.card.repository.CardXrefRepository;
import com.carddemo.common.batch.BatchCli;
import com.carddemo.common.batch.ReturnCode;
import com.carddemo.common.cobol.CodePage;
import com.carddemo.reference.repository.TranCatBalanceRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class DataLoaderTest extends PostgresTestBase {

    static final Path DATA = Path.of("..", "..", "app", "data");
    static final String ASCII = "--dd=ACCTDATA=" + DATA.resolve("ASCII/acctdata.txt");

    @Autowired
    BatchJobLauncher launcher;

    @Autowired
    AccountRepository accounts;

    @Autowired
    CardXrefRepository xrefs;

    @Autowired
    TranCatBalanceRepository balances;

    final ByteArrayOutputStream err = new ByteArrayOutputStream();

    @BeforeEach
    void clean() {
        truncateBusinessTables();
    }

    int load(String... args) {
        return BatchCli.run(args, launcher::run, new PrintStream(err, true, StandardCharsets.UTF_8));
    }

    int loadSample(String dir, String acct, String xref, String tcat, String encoding) {
        return load("--job=LOAD", "--dd=ACCTDATA=" + DATA.resolve(dir).resolve(acct),
                "--dd=CARDXREF=" + DATA.resolve(dir).resolve(xref), "--dd=TCATBALF=" + DATA.resolve(dir).resolve(tcat),
                "--dd-encoding=" + encoding);
    }

    /** An empty DALYTRAN (GC-5) leaves the tables unchanged, so load + unload must equal the GC-5 oracle unloads. */
    void assertTablesEqualGc5() {
        GoldenCase gc5 = GoldenCase.of("GC-5");
        assertThat(GoldenCompare.compareUnloads(GoldenCompare.lines(gc5.expectedTable("ACCTDATA.unl")),
                TableUnloader.accounts(accounts), 300)).isEmpty();
        assertThat(GoldenCompare.compareUnloads(GoldenCompare.lines(gc5.expectedTable("TCATBALF.unl")),
                TableUnloader.tranCatBalances(balances), 50)).isEmpty();
    }

    @Test
    void loadsTheAsciiSampleLikeTheOracle() {
        assertThat(loadSample("ASCII", "acctdata.txt", "cardxref.txt", "tcatbal.txt", "ASCII")).isEqualTo(ReturnCode.OK);
        assertThat(accounts.count()).isEqualTo(50);
        assertThat(xrefs.count()).isEqualTo(50);
        assertThat(balances.count()).isEqualTo(50);
        assertThat(accounts.findById(1L)).get().satisfies(a -> {
            assertThat(a.getCurrBal()).isEqualByComparingTo(new BigDecimal("194.00"));
            assertThat(a.getCreditLimit()).isEqualByComparingTo(new BigDecimal("2020.00"));
            assertThat(a.getOpenDate()).hasToString("2014-11-20");
            assertThat(a.getVersion()).isZero();
        });
        assertThat(xrefs.findById("0500024453765740")).get().extracting(CardXref::getAcctId).isEqualTo(50L);
        assertTablesEqualGc5();
    }

    /** The CP037 file's records, converted to ASCII, as the oracle unloader would print them. */
    static List<String> asAsciiUnload(String file, int length) throws Exception {
        byte[] ebcdic = Files.readAllBytes(DATA.resolve("EBCDIC").resolve(file));
        return GoldenCompare.records(CodePage.CP037.convert(ebcdic, CodePage.ASCII), length).stream()
                .map(r -> CodePage.ASCII.decode(r).stripTrailing()).sorted().toList();
    }

    /**
     * The EBCDIC sample is not the ASCII sample re-encoded (ACCTDATA record 49 has ADDR-ZIP 'ZEROAPR' instead of
     * 'A000000000'), so the CP037 load is checked against its own file: every record round-trips byte for byte.
     */
    @Test
    void loadsTheCp037SampleByteForByte() throws Exception {
        assertThat(loadSample("EBCDIC", "AWS.M2.CARDDEMO.ACCTDATA.PS", "AWS.M2.CARDDEMO.CARDXREF.PS",
                "AWS.M2.CARDDEMO.TCATBALF.PS", "CP037")).isEqualTo(ReturnCode.OK);
        assertThat(xrefs.count()).isEqualTo(50);
        assertThat(GoldenCompare.compareUnloads(asAsciiUnload("AWS.M2.CARDDEMO.ACCTDATA.PS", 300),
                TableUnloader.accounts(accounts), 300)).isEmpty();
        assertThat(GoldenCompare.compareUnloads(asAsciiUnload("AWS.M2.CARDDEMO.TCATBALF.PS", 50),
                TableUnloader.tranCatBalances(balances), 50)).isEmpty();
        assertThat(accounts.findById(49L)).get().extracting(a -> a.getAddrZip()).isEqualTo("ZEROAPR");
    }

    @Test
    void loadsTheGc6CustomCrossReference() {
        Path xref = GoldenCase.of("GC-6").customCardXref().orElseThrow();
        assertThat(load("--job=LOAD", "--dd=CARDXREF=" + xref)).isEqualTo(ReturnCode.OK);
        assertThat(xrefs.findById("9999000000000001")).get().extracting(CardXref::getAcctId).isEqualTo(99L);
        assertThat(accounts.existsById(99L)).isFalse();
    }

    @Test
    void aKeyAlreadyLoadedAbendsAndLoadsNothing() {
        assertThat(load("--job=LOAD", ASCII)).isEqualTo(ReturnCode.OK);
        assertThat(loadSample("ASCII", "acctdata.txt", "cardxref.txt", "tcatbal.txt", "ASCII")).isEqualTo(ReturnCode.ABEND);
        assertThat(xrefs.count()).isZero();
        assertThat(accounts.count()).isEqualTo(50);
    }

    @Test
    void usageErrorsAreRc12() {
        assertThat(load("--job=LOAD")).isEqualTo(ReturnCode.JCL_ERROR);
        assertThat(load("--job=LOAD", "--dd=TRANSACT=x")).isEqualTo(ReturnCode.JCL_ERROR);
        assertThat(err.toString(StandardCharsets.UTF_8)).contains("unknown DD TRANSACT");
    }

    @Test
    void aMissingFileIsFileStatus35Rc16() {
        assertThat(load("--job=LOAD", "--dd=ACCTDATA=/nonexistent/acctdata.txt")).isEqualTo(ReturnCode.ABEND);
    }
}
