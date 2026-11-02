package com.carddemo.transaction.batch;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.carddemo.common.batch.BatchArguments;
import com.carddemo.common.batch.BatchJobLauncher;
import com.carddemo.common.batch.BatchRun;
import com.carddemo.common.batch.ReturnCodes;
import com.carddemo.common.record.CobolRecord;
import com.carddemo.common.record.CodePage;
import com.carddemo.common.record.RecordFormat;
import com.carddemo.common.record.RecordReader;
import com.carddemo.common.record.RecordWriter;
import com.carddemo.common.record.layout.DalyrejsRecordLayout;
import com.carddemo.common.record.layout.DalytranRecordLayout;
import com.carddemo.common.record.layout.TranCatBalRecordLayout;
import com.carddemo.common.record.layout.TranRecordLayout;
import com.carddemo.transaction.repository.CategoryBalanceRepository;
import com.carddemo.transaction.repository.DailyTransactionRepository;
import com.carddemo.transaction.repository.TransactionRejectRepository;
import com.carddemo.transaction.repository.TransactionRepository;
import com.carddemo.transaction.support.StubGateways;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDate;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.batch.core.BatchStatus;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.Import;
import org.springframework.test.web.servlet.MockMvc;

/** The file adapters and launch semantics of POSTTRAN, INTCALC and COMBTRAN (not business parity). */
@SpringBootTest(properties = "carddemo.batch.output-dir=target/batch-out")
@AutoConfigureMockMvc
@Import(StubGateways.class)
class BatchJobsTest {

    private static final String CARD = "4111111111111111";
    private static final long ACCOUNT = 11111111111L;

    @TempDir
    Path work;

    @Autowired
    private BatchJobLauncher launcher;

    @Autowired
    private MockMvc mvc;

    @Autowired
    private DailyTransactionRepository dailyTransactions;

    @Autowired
    private TransactionRepository transactions;

    @Autowired
    private TransactionRejectRepository rejects;

    @Autowired
    private CategoryBalanceRepository categoryBalances;

    @Autowired
    private StubGateways.StubCardGateway cards;

    @Autowired
    private StubGateways.StubAccountGateway accounts;

    @BeforeEach
    void setUp() {
        transactions.deleteAll();
        dailyTransactions.deleteAll();
        rejects.deleteAll();
        categoryBalances.deleteAll();
        cards.clear();
        accounts.clear();
        cards.register(CARD, 100000001L, ACCOUNT);
        accounts.register(ACCOUNT, "DEFAULT", new BigDecimal("100.00"));
    }

    private static CobolRecord dalytran(String id, String card, String amount) {
        return CobolRecord.initialized(DalytranRecordLayout.LAYOUT, CodePage.ASCII)
                .set(DalytranRecordLayout.DALYTRAN_ID, id)
                .set(DalytranRecordLayout.DALYTRAN_TYPE_CD, "01")
                .set(DalytranRecordLayout.DALYTRAN_CAT_CD, 1)
                .set(DalytranRecordLayout.DALYTRAN_SOURCE, "POS TERM")
                .set(DalytranRecordLayout.DALYTRAN_DESC, "Purchase")
                .set(DalytranRecordLayout.DALYTRAN_AMT, new BigDecimal(amount))
                .set(DalytranRecordLayout.DALYTRAN_MERCHANT_ID, 800000001L)
                .set(DalytranRecordLayout.DALYTRAN_CARD_NUM, card)
                .set(DalytranRecordLayout.DALYTRAN_ORIG_TS, "2022-07-18 10:15:30.000000");
    }

    private BatchRun run(String job, Map<String, Path> dd) throws Exception {
        return launcher.launch(new BatchArguments(job, LocalDate.of(2022, 7, 18), null, work.resolve("out"), dd,
                null, null));
    }

    @Test
    void posttranReadsDalytranFromAFileAndWritesTheOutputDds() throws Exception {
        Path dalytranFile = work.resolve("DALYTRAN");
        RecordWriter.writeAll(dalytranFile, List.of(dalytran("0000000000000001", CARD, "25.00"),
                dalytran("0000000000000002", "9999999999999999", "10.00")), RecordFormat.FIXED);

        BatchRun run = run("POSTTRAN", Map.of("DALYTRAN", dalytranFile));

        assertThat(run.execution().getStatus()).isEqualTo(BatchStatus.COMPLETED);
        assertThat(run.returnCode()).isEqualTo(4);
        assertThat(run.outputDir().getParent()).isEqualTo(work.resolve("out/POSTTRAN"));
        assertThat(Files.readString(run.outputDir().resolve("RETURN-CODE"))).isEqualTo("4\n");
        assertThat(Files.size(run.outputDir().resolve("DALYREJS"))).isEqualTo(430);
        assertThat(Files.size(run.outputDir().resolve("TRANSACT"))).isEqualTo(350);
        assertThat(Files.size(run.outputDir().resolve("TCATBALF"))).isEqualTo(50);

        CobolRecord reject = RecordReader.readAll(run.outputDir().resolve("DALYREJS"), DalyrejsRecordLayout.LAYOUT,
                CodePage.ASCII, RecordFormat.FIXED).get(0);
        assertThat(reject.getInt(DalyrejsRecordLayout.WS_VALIDATION_FAIL_REASON)).isEqualTo(100);
        assertThat(DalyrejsRecordLayout.dalytran(reject).get(DalytranRecordLayout.DALYTRAN_ID))
                .isEqualTo("0000000000000002");
        CobolRecord posted = RecordReader.readAll(run.outputDir().resolve("TRANSACT"), TranRecordLayout.LAYOUT,
                CodePage.ASCII, RecordFormat.FIXED).get(0);
        assertThat(posted.get(TranRecordLayout.TRAN_ID)).isEqualTo("0000000000000001");
        assertThat(posted.get(TranRecordLayout.TRAN_AMT)).isEqualByComparingTo("25.00");
        assertThat(posted.get(TranRecordLayout.TRAN_ORIG_TS)).isEqualTo("2022-07-18 10:15:30.000000");
        CobolRecord balance = RecordReader.readAll(run.outputDir().resolve("TCATBALF"), TranCatBalRecordLayout.LAYOUT,
                CodePage.ASCII, RecordFormat.FIXED).get(0);
        assertThat(balance.getLong(TranCatBalRecordLayout.TRANCAT_ACCT_ID)).isEqualTo(ACCOUNT);
        assertThat(balance.get(TranCatBalRecordLayout.TRAN_CAT_BAL)).isEqualByComparingTo("25.00");
    }

    @Test
    void resubmittingTheSameDalytranFilePostsNothingTwice() throws Exception {
        Path dalytranFile = work.resolve("DALYTRAN");
        RecordWriter.writeAll(dalytranFile, List.of(dalytran("0000000000000001", CARD, "25.00")), RecordFormat.FIXED);
        run("POSTTRAN", Map.of("DALYTRAN", dalytranFile));

        BatchRun again = run("POSTTRAN", Map.of("DALYTRAN", dalytranFile));

        assertThat(again.returnCode()).isZero();
        assertThat(again.summary()).extractingByKey("results").asInstanceOf(
                org.assertj.core.api.InstanceOfAssertFactories.MAP)
                .containsEntry("alreadyStaged", 1).containsEntry("posted", 0);
        assertThat(Files.size(again.outputDir().resolve("DALYREJS"))).isZero();
        assertThat(accounts.postings()).hasSize(1);
    }

    @Test
    void aBoundDalytranPostsOnlyItsOwnRecordsAndKeepsRejectBytes() throws Exception {
        dailyTransactions.save(BatchRecords.toDailyTransaction(dalytran("0000000000000009", CARD, "5.00")));
        byte[] invalid = dalytran("0000000000000002", "9999999999999999", "10.00").toBytes();
        byte[] filler = "SOURCE-FEED-00000001".getBytes(java.nio.charset.StandardCharsets.US_ASCII);
        System.arraycopy(filler, 0, invalid, 330, 20);
        Path dalytranFile = work.resolve("DALYTRAN");
        Files.write(dalytranFile, invalid);

        BatchRun run = run("POSTTRAN", Map.of("DALYTRAN", dalytranFile));

        assertThat(run.returnCode()).isEqualTo(4);
        assertThat(dailyTransactions.findById("0000000000000009").orElseThrow().isProcessed()).isFalse();
        byte[] reject = Files.readAllBytes(run.outputDir().resolve("DALYREJS"));
        assertThat(java.util.Arrays.copyOf(reject, 350)).isEqualTo(invalid);
    }

    @Test
    void lineSequentialSystranIsReadBackByCombtran() throws Exception {
        Path systranDir = work.resolve("out/INTCALC/20220718-000000-000");
        Files.createDirectories(systranDir);
        RecordWriter.writeAll(systranDir.resolve("SYSTRAN.txt"), List.of(tran("0000000000000002")),
                RecordFormat.LINE_SEQUENTIAL);
        Path backup = work.resolve("TRANSACT.BKUP");
        RecordWriter.writeAll(backup, List.of(tran("0000000000000001")), RecordFormat.FIXED);

        BatchRun run = run("COMBTRAN", Map.of("TRANSACT.BKUP", backup));

        assertThat(run.returnCode()).isZero();
        assertThat(Files.size(run.outputDir().resolve("TRANSACT.COMBINED"))).isEqualTo(700);
    }

    @Test
    void anUnreadableDalytranAbendsTheJobBeforePosting() throws Exception {
        Path shortFile = work.resolve("DALYTRAN");
        Files.write(shortFile, new byte[100]);

        BatchRun run = run("POSTTRAN", Map.of("DALYTRAN", shortFile));

        assertThat(run.execution().getStatus()).isEqualTo(BatchStatus.FAILED);
        assertThat(run.returnCode()).isEqualTo(ReturnCodes.ABEND);
        assertThat(run.execution().getStepExecutions()).hasSize(1);
        assertThat(run.outputDir().resolve("DALYREJS")).doesNotExist();
    }

    @Test
    void intcalcWritesSystranAndRecordsTheParm() throws Exception {
        BatchRun run = run("INTCALC", Map.of());

        assertThat(run.returnCode()).isZero();
        assertThat(run.outputDir().resolve("SYSTRAN")).exists();
        assertThat(run.summary()).extractingByKey("results").asInstanceOf(
                org.assertj.core.api.InstanceOfAssertFactories.MAP).containsEntry("parm", "2022071800");
    }

    @Test
    void combtranSortsBackupAndSystranByTranIdAndAbendsWithoutAnInput() throws Exception {
        Path backup = work.resolve("TRANSACT.BKUP");
        Path systran = work.resolve("SYSTRAN");
        RecordWriter.writeAll(backup, List.of(tran("0000000000000003"), tran("0000000000000001")), RecordFormat.FIXED);
        RecordWriter.writeAll(systran, List.of(tran("0000000000000002")), RecordFormat.FIXED);

        BatchRun run = run("COMBTRAN", Map.of("TRANSACT.BKUP", backup, "SYSTRAN", systran));

        assertThat(run.returnCode()).isZero();
        assertThat(RecordReader.readAll(run.outputDir().resolve("TRANSACT.COMBINED"), TranRecordLayout.LAYOUT,
                CodePage.ASCII, RecordFormat.FIXED)).extracting(r -> r.get(TranRecordLayout.TRAN_ID))
                .containsExactly("0000000000000001", "0000000000000002", "0000000000000003");

        BatchRun missing = run("COMBTRAN", Map.of("SYSTRAN", systran));
        assertThat(missing.returnCode()).isEqualTo(ReturnCodes.ABEND);
    }

    @Test
    void httpLaunchStillWorksAndReportsTheOutputDirectory() throws Exception {
        mvc.perform(post("/api/v1/batch/post-transactions"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.status").value("COMPLETED"))
                .andExpect(jsonPath("$.jobName").value(BatchConfig.POST_TRANSACTIONS_JOB))
                .andExpect(jsonPath("$.returnCode").value(0))
                .andExpect(jsonPath("$.results.posted").value(0))
                .andExpect(jsonPath("$.outputDir").exists());
        mvc.perform(post("/api/v1/batch/interest-calculation").param("parm", "2022071800"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.status").value("COMPLETED"))
                .andExpect(jsonPath("$.results.parm").value("2022071800"));
    }

    private static CobolRecord tran(String id) {
        return CobolRecord.initialized(TranRecordLayout.LAYOUT, CodePage.ASCII).set(TranRecordLayout.TRAN_ID, id);
    }
}
