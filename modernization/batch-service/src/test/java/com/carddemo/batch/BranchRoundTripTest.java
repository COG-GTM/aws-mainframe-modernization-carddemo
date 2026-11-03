package com.carddemo.batch;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.batch.source.SourceTables;
import com.carddemo.common.batch.BatchArguments;
import com.carddemo.common.batch.BatchJobLauncher;
import com.carddemo.common.batch.BatchRun;
import com.carddemo.common.batch.DdFiles;
import com.carddemo.common.record.AlphanumericField;
import com.carddemo.common.record.CobolRecord;
import com.carddemo.common.record.CodePage;
import com.carddemo.common.record.RecordLayout;
import com.carddemo.common.record.layout.AccountRecordLayout;
import com.carddemo.common.record.layout.CardRecordLayout;
import com.carddemo.common.record.layout.CardXrefRecordLayout;
import com.carddemo.common.record.layout.CustomerRecordLayout;
import com.carddemo.common.record.layout.DalytranRecordLayout;
import com.carddemo.common.record.layout.TranRecordLayout;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import javax.sql.DataSource;
import org.flywaydb.core.Flyway;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.jdbc.core.JdbcTemplate;

/**
 * The ticket's round trip: the five service tables filled from the sample files, CBEXPORT reading the tables
 * (no {@code --dd}), CBIMPORT reading that EXPFILE, and each import output equal to the table it came from
 * and to the source file (FILLER aside: CBIMPORT leaves it binary zeros). CREASTMT on the tables gives the
 * same statements as the COBOL reference run on the files.
 */
@SpringBootTest(properties = "spring.main.allow-bean-definition-overriding=true")
class BranchRoundTripTest {

    static final Path DATA = GoldenSetTest.DATA;
    static final CodePage ASCII = CodePage.ASCII;
    static final LocalDateTime PROCESSED = LocalDateTime.of(2022, 7, 18, 10, 30, 45);

    /** Schema to the module whose {@code db/migration} creates it (as SampleDataReconciliationTest). */
    static final Map<String, String> SCHEMAS = Map.of("customer", "customer-service", "account", "account-service",
            "card", "card-service", "transaction", "transaction-service");

    @Autowired
    private BatchJobLauncher launcher;
    @Autowired
    private DataSource dataSource;
    @Autowired
    private JdbcTemplate jdbc;
    @Autowired
    private SourceTables tables;

    @TempDir
    Path out;

    @BeforeEach
    void loadTables() {
        SCHEMAS.keySet().forEach(schema -> jdbc.execute("DROP SCHEMA IF EXISTS " + schema + " CASCADE"));
        SCHEMAS.forEach((schema, module) -> Flyway.configure().dataSource(dataSource).schemas(schema)
                .defaultSchema(schema).createSchemas(true)
                .locations("filesystem:../" + module + "/src/main/resources/db/migration").load().migrate());
        for (CobolRecord c : file("custdata.txt", CustomerRecordLayout.LAYOUT)) {
            jdbc.update("INSERT INTO customer.customers (cust_id, first_name, middle_name, last_name, addr_line_1,"
                    + " addr_line_2, addr_line_3, addr_state_cd, addr_country_cd, addr_zip, phone_num_1, phone_num_2,"
                    + " ssn, govt_issued_id, date_of_birth, eft_account_id, pri_card_holder_ind, fico_credit_score)"
                    + " VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,CAST(? AS DATE),?,?,?)",
                    c.getLong(CustomerRecordLayout.CUST_ID), c.get(CustomerRecordLayout.CUST_FIRST_NAME),
                    c.get(CustomerRecordLayout.CUST_MIDDLE_NAME), c.get(CustomerRecordLayout.CUST_LAST_NAME),
                    c.get(CustomerRecordLayout.CUST_ADDR_LINE_1), c.get(CustomerRecordLayout.CUST_ADDR_LINE_2),
                    c.get(CustomerRecordLayout.CUST_ADDR_LINE_3), c.get(CustomerRecordLayout.CUST_ADDR_STATE_CD),
                    c.get(CustomerRecordLayout.CUST_ADDR_COUNTRY_CD), c.get(CustomerRecordLayout.CUST_ADDR_ZIP),
                    c.get(CustomerRecordLayout.CUST_PHONE_NUM_1), c.get(CustomerRecordLayout.CUST_PHONE_NUM_2),
                    c.getLong(CustomerRecordLayout.CUST_SSN), c.get(CustomerRecordLayout.CUST_GOVT_ISSUED_ID),
                    c.get(CustomerRecordLayout.CUST_DOB_YYYY_MM_DD), c.get(CustomerRecordLayout.CUST_EFT_ACCOUNT_ID),
                    c.get(CustomerRecordLayout.CUST_PRI_CARD_HOLDER_IND),
                    c.getLong(CustomerRecordLayout.CUST_FICO_CREDIT_SCORE));
        }
        for (CobolRecord a : file("acctdata.txt", AccountRecordLayout.LAYOUT)) {
            jdbc.update("INSERT INTO account.accounts (acct_id, active_status, curr_bal, credit_limit, cash_credit_limit,"
                    + " open_date, expiration_date, reissue_date, curr_cyc_credit, curr_cyc_debit, addr_zip, group_id)"
                    + " VALUES (?,?,?,?,?,CAST(? AS DATE),CAST(? AS DATE),CAST(? AS DATE),?,?,?,?)",
                    a.getLong(AccountRecordLayout.ACCT_ID), a.get(AccountRecordLayout.ACCT_ACTIVE_STATUS),
                    a.get(AccountRecordLayout.ACCT_CURR_BAL), a.get(AccountRecordLayout.ACCT_CREDIT_LIMIT),
                    a.get(AccountRecordLayout.ACCT_CASH_CREDIT_LIMIT), a.get(AccountRecordLayout.ACCT_OPEN_DATE),
                    a.get(AccountRecordLayout.ACCT_EXPIRAION_DATE), a.get(AccountRecordLayout.ACCT_REISSUE_DATE),
                    a.get(AccountRecordLayout.ACCT_CURR_CYC_CREDIT), a.get(AccountRecordLayout.ACCT_CURR_CYC_DEBIT),
                    a.get(AccountRecordLayout.ACCT_ADDR_ZIP), a.get(AccountRecordLayout.ACCT_GROUP_ID));
        }
        for (CobolRecord d : file("carddata.txt", CardRecordLayout.LAYOUT)) {
            jdbc.update("INSERT INTO card.cards (card_num, acct_id, cvv_cd, embossed_name, expiration_date, active_status)"
                    + " VALUES (?,?,?,?,CAST(? AS DATE),?)", d.get(CardRecordLayout.CARD_NUM),
                    d.getLong(CardRecordLayout.CARD_ACCT_ID), d.getLong(CardRecordLayout.CARD_CVV_CD),
                    d.get(CardRecordLayout.CARD_EMBOSSED_NAME), d.get(CardRecordLayout.CARD_EXPIRAION_DATE),
                    d.get(CardRecordLayout.CARD_ACTIVE_STATUS));
        }
        for (CobolRecord x : file("cardxref.txt", CardXrefRecordLayout.LAYOUT)) {
            jdbc.update("INSERT INTO card.card_xref (card_num, cust_id, acct_id) VALUES (?,?,?)",
                    x.get(CardXrefRecordLayout.XREF_CARD_NUM), x.getLong(CardXrefRecordLayout.XREF_CUST_ID),
                    x.getLong(CardXrefRecordLayout.XREF_ACCT_ID));
        }
        // transaction.transactions is filled by POSTTRAN; here every daily transaction is taken as posted.
        for (CobolRecord t : file("dailytran.txt", DalytranRecordLayout.LAYOUT)) {
            jdbc.update("INSERT INTO transaction.transactions (tran_id, type_cd, cat_cd, source, description, amount,"
                    + " merchant_id, merchant_name, merchant_city, merchant_zip, card_num, orig_ts, proc_ts)"
                    + " VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)", t.get(DalytranRecordLayout.DALYTRAN_ID),
                    t.get(DalytranRecordLayout.DALYTRAN_TYPE_CD), t.getLong(DalytranRecordLayout.DALYTRAN_CAT_CD),
                    t.get(DalytranRecordLayout.DALYTRAN_SOURCE), t.get(DalytranRecordLayout.DALYTRAN_DESC),
                    t.get(DalytranRecordLayout.DALYTRAN_AMT), t.getLong(DalytranRecordLayout.DALYTRAN_MERCHANT_ID),
                    t.get(DalytranRecordLayout.DALYTRAN_MERCHANT_NAME), t.get(DalytranRecordLayout.DALYTRAN_MERCHANT_CITY),
                    t.get(DalytranRecordLayout.DALYTRAN_MERCHANT_ZIP), t.get(DalytranRecordLayout.DALYTRAN_CARD_NUM),
                    LocalDateTime.parse(t.get(DalytranRecordLayout.DALYTRAN_ORIG_TS).replace(' ', 'T')), PROCESSED);
        }
    }

    @Test
    void exportThenImportGivesBackTheFiveTables() throws Exception {
        BatchRun export = launch("CBEXPORT");
        assertThat(export.returnCode()).isZero();
        assertThat(export.summary().toString()).contains("CUSTFILE=customer.customers", "EXPFILE=500");

        BatchRun imported = launch("CBIMPORT");
        assertThat(imported.returnCode()).isZero();
        Path dir = imported.outputDir();
        assertThat(Files.size(dir.resolve("ERROUT"))).isZero();

        assertSameRecords(dir.resolve("CUSTOUT"), CustomerRecordLayout.LAYOUT, CustomerRecordLayout.FILLER, tables::customers,
                file("custdata.txt", CustomerRecordLayout.LAYOUT));
        assertSameRecords(dir.resolve("ACCTOUT"), AccountRecordLayout.LAYOUT, AccountRecordLayout.FILLER, tables::accounts,
                file("acctdata.txt", AccountRecordLayout.LAYOUT));
        assertSameRecords(dir.resolve("XREFOUT"), CardXrefRecordLayout.LAYOUT, CardXrefRecordLayout.FILLER,
                tables::crossReferences, file("cardxref.txt", CardXrefRecordLayout.LAYOUT));
        assertSameRecords(dir.resolve("CARDOUT"), CardRecordLayout.LAYOUT, CardRecordLayout.FILLER, tables::cards,
                file("carddata.txt", CardRecordLayout.LAYOUT));
        // The daily file's DALYTRAN-PROC-TS is blank; the posted row carries the processing time.
        assertSameRecords(dir.resolve("TRNXOUT"), TranRecordLayout.LAYOUT, TranRecordLayout.FILLER, tables::transactions,
                null);
    }

    @Test
    void statementsFromTheTablesMatchTheCobolReference() throws Exception {
        BatchRun run = launch("CREASTMT");
        assertThat(run.returnCode()).isZero();
        Path expected = GoldenSetTest.GOLDEN.resolve("CREASTMT/expected");
        for (String dd : List.of("STMTFILE", "HTMLFILE")) {
            assertThat(Files.readString(run.outputDir().resolve(dd), StandardCharsets.ISO_8859_1)).as(dd)
                    .isEqualTo(Files.readString(expected.resolve(dd), StandardCharsets.ISO_8859_1));
        }
    }

    private BatchRun launch(String job) throws Exception {
        return launcher.launch(BatchArguments.parse("--job=" + job, "--output-dir=" + out).orElseThrow());
    }

    private static List<CobolRecord> file(String name, RecordLayout layout) {
        return DdFiles.read(DATA.resolve(name), layout, ASCII);
    }

    /** Output records equal the table unload (FILLER blank) and, when given, the source file, FILLER aside. */
    private static void assertSameRecords(Path output, RecordLayout layout, AlphanumericField filler,
                                          Function<CodePage, List<CobolRecord>> table, List<CobolRecord> source) {
        List<CobolRecord> imported = DdFiles.read(output, layout, ASCII);
        List<CobolRecord> rows = table.apply(ASCII);
        assertThat(imported).as(output.getFileName() + " count").hasSize(rows.size());
        for (int i = 0; i < imported.size(); i++) {
            byte[] got = imported.get(i).toBytes();
            byte[] fillerBytes = Arrays.copyOfRange(got, filler.offset(), filler.offset() + filler.length());
            assertThat(fillerBytes).as(output.getFileName() + " FILLER").containsOnly((byte) 0);
            assertThat(withoutFiller(got, filler)).as(output.getFileName() + " vs table, record " + (i + 1))
                    .isEqualTo(withoutFiller(rows.get(i).toBytes(), filler));
            if (source != null) {
                assertThat(withoutFiller(got, filler)).as(output.getFileName() + " vs file, record " + (i + 1))
                        .isEqualTo(withoutFiller(source.get(i).toBytes(), filler));
            }
        }
    }

    private static String withoutFiller(byte[] record, AlphanumericField filler) {
        byte[] copy = record.clone();
        Arrays.fill(copy, filler.offset(), filler.offset() + filler.length(), (byte) ' ');
        return new String(copy, StandardCharsets.ISO_8859_1);
    }
}
