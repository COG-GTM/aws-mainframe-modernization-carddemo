package com.carddemo.batch.load;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import com.carddemo.account.AccountRecord;
import com.carddemo.account.AccountRepository;
import com.carddemo.batch.exchange.ExchangeJobConfiguration;
import com.carddemo.batch.exchange.ExportRecordType;
import com.carddemo.batch.load.VsamDatasetLoader.Dataset;
import com.carddemo.card.CardRecord;
import com.carddemo.card.CardRepository;
import com.carddemo.card.CardXrefRecord;
import com.carddemo.card.CardXrefRepository;
import com.carddemo.common.codec.Field;
import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.codec.RecordLayout;
import com.carddemo.common.codec.TestData;
import com.carddemo.common.data.CopybookRecordMapper;
import com.carddemo.customer.CustomerRecord;
import com.carddemo.customer.CustomerRepository;
import com.carddemo.support.Samples;
import com.carddemo.transaction.DailyTransactionRecord;
import com.carddemo.transaction.DailyTransactionRepository;
import com.carddemo.transaction.DisclosureGroupRecord;
import com.carddemo.transaction.DisclosureGroupRepository;
import com.carddemo.transaction.TranCatBalanceRecord;
import com.carddemo.transaction.TranCatBalanceRepository;
import com.carddemo.transaction.TransactionCategoryRecord;
import com.carddemo.transaction.TransactionCategoryRepository;
import com.carddemo.transaction.TransactionRecord;
import com.carddemo.transaction.TransactionRepository;
import com.carddemo.transaction.TransactionTypeRecord;
import com.carddemo.transaction.TransactionTypeRepository;
import com.carddemo.user.UserSecurityRecord;
import com.carddemo.user.UserSecurityRepository;
import java.io.IOException;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.junit.jupiter.api.Test;
import org.springframework.batch.core.BatchStatus;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.JobParametersBuilder;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.batch.core.repository.JobInstanceAlreadyCompleteException;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.testcontainers.service.connection.ServiceConnection;
import org.springframework.data.domain.Sort;
import org.springframework.jdbc.core.JdbcTemplate;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

/**
 * Gate of step s3.3: runs {@code initial-load} over the eleven {@code app/data/EBCDIC} files into PostgreSQL 16,
 * reconciles counts and decoded values against the files, their {@code app/data/ASCII} twins and the GnuCOBOL
 * baseline prints of CBACT01C/CBACT02C/CBACT03C/CBCUS01C, proves the load idempotent, and round-trips the data
 * through {@code cbexport}/{@code cbimport}. Writes {@value #REPORT}; regenerate with
 * {@code mvn verify -Dit.test=DataLoadReconciliationIT -Dtest=none -Dsurefire.failIfNoSpecifiedTests=false
 * -Dcarddemo.docs.write=true}.
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.NONE, properties = {
        "carddemo.clock.fixed=2022-07-06T00:00:00",
        "carddemo.batch.output-dir=target/data-load-it/batch-output"})
@Testcontainers
class DataLoadReconciliationIT {

    static final String REPORT = "docs/validation/data-load/RECONCILIATION.md";
    static final String EXPECTED_DIFFS = "/codec/ebcdic-vs-ascii-expected-diffs.txt";

    /** Table per dataset (Flyway V2). */
    static final Map<Dataset, String> TABLES = new EnumMap<>(Map.ofEntries(Map.entry(Dataset.USRSEC, "user_security"),
            Map.entry(Dataset.CUSTDATA, "customer"), Map.entry(Dataset.ACCTDATA, "account"),
            Map.entry(Dataset.CARDDATA, "card"), Map.entry(Dataset.CARDXREF, "card_xref"),
            Map.entry(Dataset.TRANSACT, "transaction"), Map.entry(Dataset.DALYTRAN, "daily_transaction"),
            Map.entry(Dataset.TRANTYPE, "transaction_type"), Map.entry(Dataset.TRANCATG, "transaction_category"),
            Map.entry(Dataset.DISCGRP, "disclosure_group"), Map.entry(Dataset.TCATBALF, "tran_cat_balance")));

    /** KSDS key length ({@code KEYS(n 0)} of the IDCAMS DEFINE in app/jcl); DALYTRAN is sequential (by position). */
    static final Map<Dataset, Integer> KEY_LENGTH = new EnumMap<>(Map.of(Dataset.USRSEC, 8, Dataset.CUSTDATA, 9,
            Dataset.ACCTDATA, 11, Dataset.CARDDATA, 16, Dataset.CARDXREF, 16, Dataset.TRANSACT, 16,
            Dataset.TRANTYPE, 2, Dataset.TRANCATG, 6, Dataset.DISCGRP, 16, Dataset.TCATBALF, 17));

    @Container
    @ServiceConnection
    static PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:16-alpine");

    @Autowired
    JobLauncher jobLauncher;
    @Autowired
    @Qualifier("initialLoadJob")
    Job initialLoadJob;
    @Autowired
    @Qualifier("cbexportJob")
    Job cbexportJob;
    @Autowired
    @Qualifier("cbimportJob")
    Job cbimportJob;
    @Autowired
    JdbcTemplate jdbc;
    @Autowired
    UserSecurityRepository users;
    @Autowired
    CustomerRepository customers;
    @Autowired
    AccountRepository accounts;
    @Autowired
    CardRepository cards;
    @Autowired
    CardXrefRepository xrefs;
    @Autowired
    TransactionRepository transactions;
    @Autowired
    DailyTransactionRepository dailyTransactions;
    @Autowired
    TransactionTypeRepository types;
    @Autowired
    TransactionCategoryRepository categories;
    @Autowired
    DisclosureGroupRepository disclosureGroups;
    @Autowired
    TranCatBalanceRepository balances;

    private final StringBuilder md = new StringBuilder();

    @Test
    void initialLoadReconcilesAndRoundTrips() throws Exception {
        Path source = TestData.resolve("app/data/EBCDIC");
        md.append("# Initial load and reconciliation (step s3.3)\n\n")
                .append("Generated by `DataLoadReconciliationIT` (PostgreSQL 16 via Testcontainers, clock pinned to ")
                .append("2022-07-06). Regenerate:\n\n```\ncd modernization\n")
                .append("JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64 mvn -B verify -Dit.test=DataLoadReconciliationIT ")
                .append("-Dtest=none -Dsurefire.failIfNoSpecifiedTests=false -Dcarddemo.docs.write=true\n```\n\n")
                .append("The build fails if this file differs from what the test produces.\n\n");

        JobParameters first = InitialLoadJobConfiguration.parameters(source, LoadMode.REPLACE);
        JobExecution load = run(initialLoadJob, first);
        countsSection(load.getExecutionContext(), source);
        byteFidelitySection();
        asciiTwinSection();
        baselinePrintSection();
        samplesSection();
        idempotencySection(source, first);
        exportImportSection();

        Path report = TestData.resolve(REPORT);
        String text = md.toString();
        if (Boolean.getBoolean("carddemo.docs.write")) {
            Files.createDirectories(report.getParent());
            Files.writeString(report, text, StandardCharsets.UTF_8);
        }
        assertThat(Files.exists(report) ? Files.readString(report, StandardCharsets.UTF_8) : "")
                .as("run with -Dcarddemo.docs.write=true to regenerate " + REPORT).isEqualTo(text);
    }

    // ---- 1. counts -----------------------------------------------------------------------------------------

    private void countsSection(ExecutionContext ctx, Path source) throws IOException {
        md.append("## 1. Record counts: file = table, zero rejects\n\n")
                .append("`initial-load` (Spring Batch, one step per file, `REPLACE` = truncate-and-load) followed by its ")
                .append("`reconcile-counts` step. *File records* = file bytes / LRECL; *empty* = all-LOW-VALUES ")
                .append("priming records, which IDCAMS REPRO writes only so the KSDS can be opened and which hold no ")
                .append("row.\n\n")
                .append("| # | File (`app/data/EBCDIC/`) | Replaces | Table | LRECL | File records | Empty | Rejected | ")
                .append("Loaded | Table rows | Result |\n|---|---|---|---|---|---|---|---|---|---|---|\n");
        int n = 0;
        for (InitialLoadInput input : InitialLoadInput.values()) {
            Dataset ds = input.dataset();
            int lrecl = ds.mapper().layout().length();
            long fileRecords = Files.size(source.resolve(input.fileName())) / lrecl;
            long read = ctx.getLong(input + ".read");
            long empty = ctx.getLong(input + ".empty");
            long rejects = ctx.getLong(input + ".rejects");
            long loaded = ctx.getLong(input + ".loaded");
            long rows = tableRows(ds);
            boolean ok = read == fileRecords && rejects == 0 && rows == loaded && loaded == read - empty;
            md.append("| ").append(++n).append(" | `").append(input.fileName()).append("` | ").append(input.jclJob())
                    .append(" | `").append(TABLES.get(ds)).append("` | ").append(lrecl).append(" | ")
                    .append(fileRecords).append(" | ").append(empty).append(" | ").append(rejects).append(" | ")
                    .append(loaded).append(" | ").append(rows).append(" | ").append(ok ? "OK" : "**MISMATCH**")
                    .append(" |\n");
            assertThat(ok).as(input + " counts").isTrue();
            assertThat(empty).as(input + " empty records").isEqualTo(ds == Dataset.TRANSACT ? 1 : 0);
        }
        assertThat(Files.mismatch(source.resolve("AWS.M2.CARDDEMO.ACCDATA.PS"),
                source.resolve("AWS.M2.CARDDEMO.ACCTDATA.PS"))).isEqualTo(-1L);
        md.append("\nNot loaded: `AWS.M2.CARDDEMO.ACCDATA.PS` is byte-identical to `ACCTDATA.PS` (verified); ")
                .append("`AWS.M2.CARDDEMO.EXPORT.DATA.PS` is CBIMPORT input, not a table image (section 7).\n\n");
    }

    // ---- 2. bytes ------------------------------------------------------------------------------------------

    private void byteFidelitySection() {
        md.append("## 2. Stored rows re-encode to the source bytes\n\n")
                .append("Every row is read back through JPA, turned into its copybook record (`toRecord()` + ")
                .append("`CopybookRecordMapper`, IBM-037) and compared with the source record of the same key. ")
                .append("FILLER is not stored (`db/copybook-column-map.csv`) and re-encodes as spaces, so ")
                .append("TRANTYPE, TRANCATG, DISCGRP and TCATBALF, whose FILLER holds zeros (`X'F0'`) in the ")
                .append("sample files, are identical only with FILLER blanked on both sides.\n\n")
                .append("| Dataset | Records | Byte-identical | Identical with FILLER blanked |\n|---|---|---|---|\n");
        for (InitialLoadInput input : InitialLoadInput.values()) {
            Dataset ds = input.dataset();
            if (ds == Dataset.TRANSACT) {
                continue;
            }
            List<FixedWidthRecord> file = Samples.read(ds, RecordEncoding.EBCDIC);
            Map<String, FixedWidthRecord> db = keyed(ds, dbImages(ds, RecordEncoding.EBCDIC));
            int same = 0;
            int sameData = 0;
            for (int i = 0; i < file.size(); i++) {
                FixedWidthRecord stored = db.get(key(ds, file.get(i), i));
                if (stored != null && Arrays.equals(stored.bytes(), file.get(i).bytes())) {
                    same++;
                }
                if (stored != null && Arrays.equals(withoutFiller(ds, stored), withoutFiller(ds, file.get(i)))) {
                    sameData++;
                }
            }
            md.append("| ").append(ds).append(" | ").append(file.size()).append(" | ").append(same).append(" | ")
                    .append(sameData).append(" |\n");
            assertThat(sameData).as(ds + " rows identical apart from FILLER").isEqualTo(file.size());
        }
        md.append("| TRANSACT | 1 priming record | n/a | n/a (no row; see section 1) |\n\n");
    }

    private static byte[] withoutFiller(Dataset ds, FixedWidthRecord record) {
        CopybookRecordMapper<?> mapper = ds.mapper();
        byte[] image = FixedWidthRecord.spaces(mapper.layout(), record.encoding()).bytes();
        byte[] source = record.bytes();
        for (RecordLayout.Leaf leaf : mapper.layout().leaves()) {
            if (mapper.fieldToComponent().containsKey(leaf.key())) {
                System.arraycopy(source, leaf.field().offset(), image, leaf.field().offset(), leaf.field().size());
            }
        }
        return image;
    }

    // ---- 3. ASCII twins ------------------------------------------------------------------------------------

    private void asciiTwinSection() throws IOException {
        List<String> expected = expectedDiffs();
        md.append("## 3. Decoded values vs the `app/data/ASCII` twins\n\n")
                .append("Each ASCII record is decoded with the same copybook and every elementary item compared with ")
                .append("the stored row of the same key (numbers as `BigDecimal`, text with trailing spaces ")
                .append("ignored). USRSEC and TRANSACT have no ASCII twin.\n\n")
                .append("| Dataset | Records | Items compared | Differences | Expected (known source-data diffs) |\n")
                .append("|---|---|---|---|---|\n");
        List<String> found = new ArrayList<>();
        for (Dataset ds : Samples.withAsciiSample()) {
            List<FixedWidthRecord> twin = Samples.read(ds, RecordEncoding.ASCII);
            Map<String, FixedWidthRecord> db = keyed(ds, dbImages(ds, RecordEncoding.ASCII));
            RecordLayout layout = ds.mapper().layout();
            int items = 0;
            List<String> diffs = new ArrayList<>();
            for (int i = 0; i < twin.size(); i++) {
                FixedWidthRecord stored = db.get(key(ds, twin.get(i), i));
                assertThat(stored).as(ds + " ASCII record " + (i + 1) + " has a row").isNotNull();
                Map<String, Object> a = layout.decode(stored);
                Map<String, Object> b = layout.decode(twin.get(i));
                for (String item : a.keySet()) {
                    items++;
                    if (!same(a.get(item), b.get(item))) {
                        diffs.add(ds + "|" + (i + 1) + "|" + item + "|" + show(a.get(item)) + "|" + show(b.get(item)));
                    }
                }
            }
            found.addAll(diffs);
            long known = diffs.stream().filter(expected::contains).count();
            md.append("| ").append(ds).append(" | ").append(twin.size()).append(" | ").append(items).append(" | ")
                    .append(diffs.size()).append(" | ").append(known).append(" |\n");
        }
        List<String> expectedForLoaded = expected.stream().filter(e -> !e.startsWith("ACCDATA|")).toList();
        assertThat(found).as("differences vs ASCII twins").containsExactlyInAnyOrderElementsOf(expectedForLoaded);
        md.append("\nThe differences are exactly the ones listed in ")
                .append("`carddemo-app/src/test/resources/codec/ebcdic-vs-ascii-expected-diffs.txt` (the ASCII copy ")
                .append("was translated from a different extract; the database holds the EBCDIC values):\n\n")
                .append("| Dataset | Record | Item | PostgreSQL (from EBCDIC) | ASCII twin |\n|---|---|---|---|---|\n");
        for (String d : found) {
            md.append("| ").append(String.join(" | ", d.split("\\|"))).append(" |\n");
        }
        md.append('\n');
    }

    // ---- 4. baseline prints --------------------------------------------------------------------------------

    private void baselinePrintSection() throws IOException {
        md.append("## 4. Stored values vs the GnuCOBOL baseline prints (phase 1)\n\n")
                .append("The baseline programs ran on the ASCII samples (`docs/validation/baseline/`). CBACT01C ")
                .append("DISPLAYs named account items; CBACT02C, CBACT03C and CBCUS01C DISPLAY whole records ")
                .append("(CBACT03C and CBCUS01C print each record twice; duplicates are collapsed). Every printed ")
                .append("item is compared with the stored row of the same key.\n\n")
                .append("| Program | Baseline | Dataset | Records printed | Matched rows | Items compared | Differences |\n")
                .append("|---|---|---|---|---|---|---|\n");
        printedAccounts();
        printedRecords("CBACT02C", "READCARD", Dataset.CARDDATA);
        printedRecords("CBACT03C", "READXREF", Dataset.CARDXREF);
        printedRecords("CBCUS01C", "READCUST", Dataset.CUSTDATA);
        md.append('\n');
    }

    private void printedAccounts() throws IOException {
        Map<String, Map<String, String>> printed = cbact01cAccounts();
        RecordLayout layout = Dataset.ACCTDATA.mapper().layout();
        Map<String, FixedWidthRecord> db = keyed(Dataset.ACCTDATA, dbImages(Dataset.ACCTDATA, RecordEncoding.ASCII));
        int items = 0;
        int matched = 0;
        List<String> diffs = new ArrayList<>();
        for (Map.Entry<String, Map<String, String>> account : printed.entrySet()) {
            FixedWidthRecord stored = db.get(account.getKey());
            if (stored == null) {
                diffs.add(account.getKey() + " not in table");
                continue;
            }
            matched++;
            Map<String, Object> values = layout.decode(stored);
            for (Map.Entry<String, String> item : account.getValue().entrySet()) {
                items++;
                Object print = parsePrinted(item.getValue(), layout.field(item.getKey()));
                if (!same(values.get(item.getKey()), print)) {
                    diffs.add(account.getKey() + " " + item.getKey());
                }
            }
        }
        row("CBACT01C", "READACCT", Dataset.ACCTDATA, printed.size(), matched, items, diffs);
    }

    private void printedRecords(String program, String job, Dataset ds) throws IOException {
        RecordLayout layout = ds.mapper().layout();
        List<String> lines = printedLines(job);
        Map<String, FixedWidthRecord> db = keyed(ds, dbImages(ds, RecordEncoding.ASCII));
        int items = 0;
        int matched = 0;
        List<String> diffs = new ArrayList<>();
        for (String line : lines) {
            FixedWidthRecord print = FixedWidthRecord.fromLine(layout, line, RecordEncoding.ASCII);
            FixedWidthRecord stored = db.get(key(ds, print, -1));
            if (stored == null) {
                diffs.add(key(ds, print, -1) + " not in table");
                continue;
            }
            matched++;
            Map<String, Object> a = layout.decode(stored);
            Map<String, Object> b = layout.decode(print);
            for (String item : a.keySet()) {
                items++;
                if (!same(a.get(item), b.get(item))) {
                    diffs.add(key(ds, print, -1) + " " + item);
                }
            }
        }
        row(program, job, ds, lines.size(), matched, items, diffs);
    }

    private void row(String program, String job, Dataset ds, int printed, int matched, int items, List<String> diffs) {
        md.append("| ").append(program).append(" | `").append(job).append("/sysout.txt` | ").append(ds).append(" | ")
                .append(printed).append(" | ").append(matched).append(" | ").append(items).append(" | ")
                .append(diffs.size()).append(" |\n");
        assertThat(diffs).as(program + " differences").isEmpty();
        assertThat(printed).as(program + " records").isEqualTo(matched).isPositive();
    }

    // ---- 5. samples ----------------------------------------------------------------------------------------

    private void samplesSection() throws IOException {
        md.append("## 5. Sampled values\n\n")
                .append("PostgreSQL values are read with plain JDBC (`numeric` columns arrive as `BigDecimal`).\n\n")
                .append("### Accounts (`account`)\n\n")
                .append("| Rec | acct_id | curr_bal PG | ASCII | CBACT01C | credit_limit PG | open_date PG | ASCII | ")
                .append("CBACT01C | addr_zip PG | ASCII |\n|---|---|---|---|---|---|---|---|---|---|---|\n");
        List<FixedWidthRecord> acctTwin = Samples.read(Dataset.ACCTDATA, RecordEncoding.ASCII);
        RecordLayout acct = Dataset.ACCTDATA.mapper().layout();
        Map<String, Map<String, String>> cbact01c = cbact01cAccounts();
        for (int rec : List.of(1, 11, 21, 31, 41, 49, 50)) {
            Map<String, Object> twin = acct.decode(acctTwin.get(rec - 1));
            long id = ((BigDecimal) twin.get("ACCT-ID")).longValueExact();
            Map<String, Object> pg = jdbc.queryForMap(
                    "select curr_bal, credit_limit, open_date, addr_zip from account where acct_id = ?", id);
            assertThat(pg.get("curr_bal")).isInstanceOf(BigDecimal.class);
            Map<String, String> print = cbact01c.get(String.format("%011d", id));
            BigDecimal printedBal = (BigDecimal) parsePrinted(print.get("ACCT-CURR-BAL"), acct.field("ACCT-CURR-BAL"));
            assertThat((BigDecimal) pg.get("curr_bal")).isEqualByComparingTo((BigDecimal) twin.get("ACCT-CURR-BAL"))
                    .isEqualByComparingTo(printedBal);
            assertThat(pg.get("open_date")).isEqualTo(twin.get("ACCT-OPEN-DATE")).isEqualTo(print.get("ACCT-OPEN-DATE"));
            md.append("| ").append(rec).append(" | ").append(id).append(" | ").append(show(pg.get("curr_bal")))
                    .append(" | ").append(show(twin.get("ACCT-CURR-BAL"))).append(" | ").append(show(printedBal))
                    .append(" | ").append(show(pg.get("credit_limit"))).append(" | ").append(pg.get("open_date"))
                    .append(" | ").append(twin.get("ACCT-OPEN-DATE")).append(" | ").append(print.get("ACCT-OPEN-DATE"))
                    .append(" | ").append(show(pg.get("addr_zip"))).append(" | ").append(show(twin.get("ACCT-ADDR-ZIP")))
                    .append(" |\n");
        }

        md.append("\n### Customers (`customer`)\n\n")
                .append("| Rec | cust_id | first_name last_name PG | ASCII | CBCUS01C | dob PG | ASCII | CBCUS01C | ")
                .append("fico PG |\n|---|---|---|---|---|---|---|---|---|\n");
        List<FixedWidthRecord> custTwin = Samples.read(Dataset.CUSTDATA, RecordEncoding.ASCII);
        RecordLayout cust = Dataset.CUSTDATA.mapper().layout();
        Map<String, Map<String, Object>> cbcus01c = printedByKey("READCUST", Dataset.CUSTDATA);
        for (int rec : List.of(1, 10, 25, 50)) {
            Map<String, Object> twin = cust.decode(custTwin.get(rec - 1));
            int id = ((BigDecimal) twin.get("CUST-ID")).intValueExact();
            Map<String, Object> pg = jdbc.queryForMap(
                    "select first_name, last_name, dob, fico_credit_score from customer where cust_id = ?", id);
            Map<String, Object> print = cbcus01c.get(String.format("%09d", id));
            String pgName = pg.get("first_name") + " " + pg.get("last_name");
            String twinName = show(twin.get("CUST-FIRST-NAME")) + " " + show(twin.get("CUST-LAST-NAME"));
            String printName = show(print.get("CUST-FIRST-NAME")) + " " + show(print.get("CUST-LAST-NAME"));
            assertThat(pgName).isEqualTo(twinName).isEqualTo(printName);
            assertThat(pg.get("dob")).isEqualTo(twin.get("CUST-DOB-YYYY-MM-DD"))
                    .isEqualTo(print.get("CUST-DOB-YYYY-MM-DD"));
            md.append("| ").append(rec).append(" | ").append(id).append(" | ").append(pgName).append(" | ")
                    .append(twinName).append(" | ").append(printName).append(" | ").append(pg.get("dob")).append(" | ")
                    .append(twin.get("CUST-DOB-YYYY-MM-DD")).append(" | ").append(print.get("CUST-DOB-YYYY-MM-DD"))
                    .append(" | ").append(pg.get("fico_credit_score")).append(" |\n");
        }

        md.append("\n### Cards (`card`)\n\n")
                .append("| Rec | card_num | embossed_name PG | ASCII | CBACT02C | expiration_date PG | ASCII | CBACT02C |\n")
                .append("|---|---|---|---|---|---|---|---|\n");
        List<FixedWidthRecord> cardTwin = Samples.read(Dataset.CARDDATA, RecordEncoding.ASCII);
        RecordLayout card = Dataset.CARDDATA.mapper().layout();
        Map<String, Map<String, Object>> cbact02c = printedByKey("READCARD", Dataset.CARDDATA);
        for (int rec : List.of(1, 25, 50)) {
            Map<String, Object> twin = card.decode(cardTwin.get(rec - 1));
            String num = (String) twin.get("CARD-NUM");
            Map<String, Object> pg = jdbc.queryForMap(
                    "select embossed_name, expiration_date from card where card_num = ?", num);
            Map<String, Object> print = cbact02c.get(num);
            assertThat(pg.get("embossed_name")).isEqualTo(show(twin.get("CARD-EMBOSSED-NAME")))
                    .isEqualTo(show(print.get("CARD-EMBOSSED-NAME")));
            md.append("| ").append(rec).append(" | ").append(num).append(" | ").append(pg.get("embossed_name"))
                    .append(" | ").append(show(twin.get("CARD-EMBOSSED-NAME"))).append(" | ")
                    .append(show(print.get("CARD-EMBOSSED-NAME"))).append(" | ").append(pg.get("expiration_date"))
                    .append(" | ").append(twin.get("CARD-EXPIRAION-DATE")).append(" | ")
                    .append(print.get("CARD-EXPIRAION-DATE")).append(" |\n");
        }
        md.append('\n');
    }

    // ---- 6. idempotency ------------------------------------------------------------------------------------

    private void idempotencySection(Path source, JobParameters first) throws Exception {
        Map<Dataset, String> before = digests();
        assertThatThrownBy(() -> jobLauncher.run(initialLoadJob, first))
                .isInstanceOf(JobInstanceAlreadyCompleteException.class);
        run(initialLoadJob, new JobParametersBuilder(first).addLong("run.id", 2L).toJobParameters());
        Map<Dataset, String> afterReplace = digests();
        run(initialLoadJob, new JobParametersBuilder(InitialLoadJobConfiguration.parameters(source, LoadMode.UPSERT))
                .toJobParameters());
        Map<Dataset, String> afterUpsert = digests();
        assertThat(afterReplace).isEqualTo(before);
        assertThat(afterUpsert).isEqualTo(before);
        md.append("## 6. Idempotency\n\n")
                .append("Table contents are fingerprinted as `count(*)` + md5 of all rows (`string_agg(t::text)` in a ")
                .append("fixed order) after the first load, after a forced second `REPLACE` run (`run.id=2`) and ")
                .append("after an `UPSERT` run (`carddemo.initial-load.mode=upsert`). Relaunching with the first ")
                .append("run's parameters is refused (`JobInstanceAlreadyCompleteException`), which is how the ")
                .append("`local`-profile startup runner skips data it has already loaded.\n\n")
                .append("| Table | Rows | Fingerprint unchanged after REPLACE rerun | after UPSERT |\n|---|---|---|---|\n");
        for (InitialLoadInput input : InitialLoadInput.values()) {
            Dataset ds = input.dataset();
            md.append("| `").append(TABLES.get(ds)).append("` | ").append(tableRows(ds)).append(" | ")
                    .append(before.get(ds).equals(afterReplace.get(ds)) ? "yes" : "**no**").append(" | ")
                    .append(before.get(ds).equals(afterUpsert.get(ds)) ? "yes" : "**no**").append(" |\n");
        }
        md.append('\n');
    }

    // ---- 7. export / import --------------------------------------------------------------------------------

    private void exportImportSection() throws Exception {
        Map<Dataset, String> before = digests();
        JobExecution export = run(cbexportJob, new JobParametersBuilder().addString("encoding", "EBCDIC")
                .addLong("run.id", 1L).toJobParameters());
        ExecutionContext exp = export.getStepExecutions().iterator().next().getExecutionContext();
        Path exportFile = Path.of(exp.getString("file"));
        JobExecution imported = run(cbimportJob, new JobParametersBuilder()
                .addString(ExchangeJobConfiguration.INPUT_FILE, exportFile.toString())
                .addString(ExchangeJobConfiguration.LOAD_MODE, "REPLACE").addLong("run.id", 1L).toJobParameters());
        ExecutionContext imp = imported.getStepExecutions().iterator().next().getExecutionContext();
        Map<Dataset, String> after = digests();

        md.append("## 7. Export / import round trip (CBEXPORT / CBIMPORT, `CVEXPORT`)\n\n")
                .append("`cbexport` wrote one `EXPORT.DATA` generation (500-byte IBM-037 records, registered in ")
                .append("`batch_output_file`, ADR-0012) from PostgreSQL; `cbimport` split it into the dated ")
                .append("`*.IMPORT` generations in the original layouts and, with `load-mode=REPLACE`, loaded them ")
                .append("back. Each import file is compared byte for byte with the original `.PS` file.\n\n")
                .append("| Type | Dataset | Exported | Imported | Import file vs original `.PS` | Table fingerprint after ")
                .append("reload |\n|---|---|---|---|---|---|\n");
        for (ExportRecordType type : ExportRecordType.values()) {
            Dataset ds = type.dataset();
            Path out = Path.of(imp.getString(type + ".file"));
            String compare;
            if (ds == Dataset.TRANSACT) {
                assertThat(Files.size(out)).isZero();
                compare = "empty (TRANSACT holds no rows after initial load)";
            } else {
                long mismatch = Files.mismatch(out, Samples.path(ds, RecordEncoding.EBCDIC));
                assertThat(mismatch).as(type + " import vs original").isEqualTo(-1L);
                compare = "**byte-identical** (" + Files.size(out) + " bytes)";
            }
            assertThat(after.get(ds)).as(type + " reload").isEqualTo(before.get(ds));
            md.append("| ").append(type.code()).append(" | ").append(ds).append(" | ")
                    .append(exp.getLong(type + ".exported")).append(" | ").append(imp.getLong(type + ".imported"))
                    .append(" | ").append(compare).append(" | unchanged |\n");
        }
        assertThat(imp.getLong("errors")).isZero();
        md.append("\nImport errors (`IMPORT.ERRORS`): ").append(imp.getLong("errors")).append(". The record-level codec ")
                .append("is also checked without a database in `ExportRecordCodecTest`: export then import is ")
                .append("byte-identical for all five types in both encodings (TRANSACT via the 300 DALYTRAN ")
                .append("records, same 350-byte layout), and the ASCII export of customers, accounts and ")
                .append("cross-references equals the GnuCOBOL CBEXPORT records 1-150 in ")
                .append("`docs/validation/baseline/CBEXPORT/EXPORT.ksds.txt` (account balance and cycle ")
                .append("credit/debit masked: the baseline exported after POSTTRAN/INTCALC had posted).\n\n");

        Path shipped = TestData.resolve("app/data/EBCDIC/AWS.M2.CARDDEMO.EXPORT.DATA.PS");
        JobExecution shippedImport = run(cbimportJob, new JobParametersBuilder()
                .addString(ExchangeJobConfiguration.INPUT_FILE, shipped.toString()).addLong("run.id", 2L)
                .toJobParameters());
        ExecutionContext si = shippedImport.getStepExecutions().iterator().next().getExecutionContext();
        md.append("### Shipped `AWS.M2.CARDDEMO.EXPORT.DATA.PS` through `cbimport`\n\n")
                .append("The shipped export (500 records, timestamp 2025-09-28) imports with ")
                .append(si.getLong("errors")).append(" errors. It was taken from a different data state, so it ")
                .append("is informational only:\n\n| Type | Imported | Records identical to the original `.PS` |\n")
                .append("|---|---|---|\n");
        assertThat(si.getLong("errors")).isZero();
        for (ExportRecordType type : ExportRecordType.values()) {
            String identical = "n/a";
            if (type.dataset() != Dataset.TRANSACT) {
                List<FixedWidthRecord> out = RecordFiles(type, Path.of(si.getString(type + ".file")));
                List<FixedWidthRecord> orig = Samples.read(type.dataset(), RecordEncoding.EBCDIC);
                int same = 0;
                for (int i = 0; i < Math.min(out.size(), orig.size()); i++) {
                    if (Arrays.equals(out.get(i).bytes(), orig.get(i).bytes())) {
                        same++;
                    }
                }
                identical = same + " / " + orig.size();
            }
            md.append("| ").append(type.code()).append(" ").append(type.dataset()).append(" | ")
                    .append(si.getLong(type + ".imported")).append(" | ").append(identical).append(" |\n");
        }
    }

    // ---- helpers -------------------------------------------------------------------------------------------

    private JobExecution run(Job job, JobParameters parameters) throws Exception {
        JobExecution execution = jobLauncher.run(job, parameters);
        assertThat(execution.getStatus()).as(job.getName() + " " + execution.getAllFailureExceptions())
                .isEqualTo(BatchStatus.COMPLETED);
        return execution;
    }

    private static List<FixedWidthRecord> RecordFiles(ExportRecordType type, Path file) {
        return com.carddemo.common.file.RecordFiles.readFixed(type.ddname(), file, type.datasetLayout(),
                RecordEncoding.EBCDIC);
    }

    private long tableRows(Dataset ds) {
        return jdbc.queryForObject("select count(*) from \"" + TABLES.get(ds) + "\"", Long.class);
    }

    private Map<Dataset, String> digests() {
        Map<Dataset, String> out = new EnumMap<>(Dataset.class);
        for (Dataset ds : TABLES.keySet()) {
            out.put(ds, jdbc.queryForObject("select count(*) || ':' || md5(coalesce(string_agg(t::text, '|' order by "
                    + "t::text), '')) from \"" + TABLES.get(ds) + "\" t", String.class));
        }
        return out;
    }

    private List<FixedWidthRecord> dbImages(Dataset ds, RecordEncoding enc) {
        return switch (ds) {
            case USRSEC -> images(users.findAll(), e -> UserSecurityRecord.MAPPER.toRecord(e.toRecord(), enc));
            case CUSTDATA -> images(customers.findAll(), e -> CustomerRecord.MAPPER.toRecord(e.toRecord(), enc));
            case ACCTDATA -> images(accounts.findAll(), e -> AccountRecord.MAPPER.toRecord(e.toRecord(), enc));
            case CARDDATA -> images(cards.findAll(), e -> CardRecord.MAPPER.toRecord(e.toRecord(), enc));
            case CARDXREF -> images(xrefs.findAll(), e -> CardXrefRecord.MAPPER.toRecord(e.toRecord(), enc));
            case TRANSACT -> images(transactions.findAll(), e -> TransactionRecord.MAPPER.toRecord(e.toRecord(), enc));
            case DALYTRAN -> images(dailyTransactions.findAll(Sort.by("recordSeq")),
                    e -> DailyTransactionRecord.MAPPER.toRecord(e.toRecord(), enc));
            case TRANTYPE -> images(types.findAll(), e -> TransactionTypeRecord.MAPPER.toRecord(e.toRecord(), enc));
            case TRANCATG -> images(categories.findAll(),
                    e -> TransactionCategoryRecord.MAPPER.toRecord(e.toRecord(), enc));
            case DISCGRP -> images(disclosureGroups.findAll(),
                    e -> DisclosureGroupRecord.MAPPER.toRecord(e.toRecord(), enc));
            case TCATBALF -> images(balances.findAll(), e -> TranCatBalanceRecord.MAPPER.toRecord(e.toRecord(), enc));
        };
    }

    private static <E> List<FixedWidthRecord> images(List<E> rows, Function<E, FixedWidthRecord> toImage) {
        return rows.stream().map(toImage).toList();
    }

    private static Map<String, FixedWidthRecord> keyed(Dataset ds, List<FixedWidthRecord> images) {
        Map<String, FixedWidthRecord> out = new LinkedHashMap<>();
        for (int i = 0; i < images.size(); i++) {
            out.put(key(ds, images.get(i), i), images.get(i));
        }
        return out;
    }

    /** KSDS key text, or the 0-based position for the sequential DALYTRAN file. */
    private static String key(Dataset ds, FixedWidthRecord record, int position) {
        Integer length = KEY_LENGTH.get(ds);
        return length == null ? Integer.toString(position) : record.text().substring(0, length);
    }

    private static boolean same(Object a, Object b) {
        if (a instanceof BigDecimal x && b instanceof BigDecimal y) {
            return x.compareTo(y) == 0;
        }
        if (a instanceof String x && b instanceof String y) {
            return x.stripTrailing().equals(y.stripTrailing());
        }
        return Objects.equals(a, b);
    }

    private static String show(Object value) {
        if (value instanceof BigDecimal d) {
            return d.toPlainString();
        }
        return value == null ? "null" : value.toString().stripTrailing();
    }

    private static List<String> expectedDiffs() throws IOException {
        try (var in = DataLoadReconciliationIT.class.getResourceAsStream(EXPECTED_DIFFS)) {
            return new String(Objects.requireNonNull(in).readAllBytes(), StandardCharsets.UTF_8).lines()
                    .filter(l -> !l.isBlank() && !l.startsWith("#")).toList();
        }
    }

    private static final Pattern ITEM = Pattern.compile("^(ACCT-[A-Z-]+?)\\s*:(.*)$");

    /** CBACT01C {@code 1100-DISPLAY-ACCT-RECORD} blocks, by ACCT-ID. */
    private static Map<String, Map<String, String>> cbact01cAccounts() throws IOException {
        Map<String, Map<String, String>> out = new LinkedHashMap<>();
        Map<String, String> current = null;
        for (String line : Files.readAllLines(TestData.resolve("docs/validation/baseline/READACCT/sysout.txt"),
                StandardCharsets.ISO_8859_1)) {
            Matcher m = ITEM.matcher(line);
            if (!m.matches()) {
                continue;
            }
            if (m.group(1).equals("ACCT-ID")) {
                current = new LinkedHashMap<>();
                out.put(m.group(2), current);
            }
            if (current != null) {
                current.put(m.group(1), m.group(2));
            }
        }
        return out;
    }

    /** DISPLAY of a signed numeric item ({@code 000000019400+}) or text, as {@link RecordLayout#decode} gives it. */
    private static Object parsePrinted(String text, Field field) {
        if (!field.isNumeric()) {
            return text;
        }
        String digits = text;
        boolean negative = false;
        if (text.endsWith("+") || text.endsWith("-")) {
            digits = text.substring(0, text.length() - 1);
            negative = text.endsWith("-");
        }
        BigDecimal value = new BigDecimal(digits).movePointLeft(field.scale());
        return negative ? value.negate() : value;
    }

    /** Distinct record lines of a whole-record DISPLAY job sysout, in print order. */
    private static List<String> printedLines(String job) throws IOException {
        LinkedHashSet<String> lines = new LinkedHashSet<>();
        for (String line : Files.readAllLines(TestData.resolve("docs/validation/baseline/" + job + "/sysout.txt"),
                StandardCharsets.ISO_8859_1)) {
            if (!line.isEmpty() && Character.isDigit(line.charAt(0)) && !line.startsWith("rc=")) {
                lines.add(line);
            }
        }
        return new ArrayList<>(lines);
    }

    private static Map<String, Map<String, Object>> printedByKey(String job, Dataset ds) throws IOException {
        Map<String, Map<String, Object>> out = new LinkedHashMap<>();
        RecordLayout layout = ds.mapper().layout();
        for (String line : printedLines(job)) {
            FixedWidthRecord print = FixedWidthRecord.fromLine(layout, line, RecordEncoding.ASCII);
            out.put(key(ds, print, -1), layout.decode(print));
        }
        return out;
    }
}
