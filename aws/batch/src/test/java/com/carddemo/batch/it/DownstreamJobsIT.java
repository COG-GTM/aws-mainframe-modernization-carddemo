package com.carddemo.batch.it;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.batch.core.ReturnCode;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.zip.GZIPInputStream;
import org.apache.pdfbox.Loader;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.text.PDFTextStripper;
import org.junit.jupiter.api.Test;

/** Month-start / weekly / on-demand jobs of the daily cycle, run after POSTTRAN on the sample data. */
class DownstreamJobsIT extends AbstractBatchIT {

    private final ObjectMapper mapper = new ObjectMapper();

    JsonNode result(String runId, String job) throws IOException {
        return mapper.readTree(read("runs/" + runId + "/" + job + ".json"));
    }

    @Test
    void monthStartChainBackupInterestCombineStatements() throws IOException {
        assertThat(run("post-daily-transactions", "d1")).isEqualTo(ReturnCode.WARNING);
        assertThat(run("backup-transactions", "d1")).isEqualTo(ReturnCode.OK);
        String backupKey = "backup/transaction/2022-07-18/d1.csv.gz";
        List<String> csv = lines(gunzip(readBytes(backupKey)));
        assertThat(csv.get(0)).startsWith("tran_id,type_cd,cat_cd");
        assertThat(csv).hasSize(1 + 262);

        assertThat(run("calculate-interest", "d1", "--parmDate=2022071800")).isEqualTo(ReturnCode.OK);
        assertThat(run("combine-transactions", "d1", "--backupKey=" + backupKey)).isEqualTo(ReturnCode.OK);
        JsonNode comb = result("d1", "combine-transactions");
        assertThat(comb.at("/counts/systemTransactions").asInt()).isEqualTo(50);
        assertThat(comb.at("/counts/systemTransactionsMissing").asInt()).isZero();
        assertThat(comb.at("/counts/transactionRows").asInt()).isEqualTo(312);
        assertThat(comb.at("/counts/backupRows").asInt()).isEqualTo(262);

        jdbc.update("DELETE FROM transaction WHERE tran_id = '2022071800000001'");
        assertThat(run("combine-transactions", "d1-broken", "--backupKey=" + backupKey))
                .isEqualTo(ReturnCode.DATA_ERROR);
        assertThat(result("d1-broken", "combine-transactions").at("/counts/systemTransactionsMissing").asInt())
                .isEqualTo(1);
    }

    @Test
    void statementsTextHtmlAndPdf() throws IOException {
        assertThat(run("post-daily-transactions", "s1")).isEqualTo(ReturnCode.WARNING);
        assertThat(run("create-statements", "s1")).isEqualTo(ReturnCode.OK);
        int xrefs = jdbc.queryForObject("SELECT count(*) FROM card_xref", Integer.class);
        assertThat(result("s1", "create-statements").at("/counts/statements").asInt()).isEqualTo(xrefs);
        assertThat(result("s1", "create-statements").at("/counts/transactions").asInt()).isEqualTo(262);

        String text = read("statements/2022-07-18/s1/statement.txt");
        List<String> lines = text.lines().toList();
        assertThat(lines).allSatisfy(l -> assertThat(l).hasSize(80));
        assertThat(lines.stream().filter(l -> l.contains("START OF STATEMENT")).count()).isEqualTo(xrefs);
        assertThat(lines.get(0)).isEqualTo("*".repeat(31) + "START OF STATEMENT" + "*".repeat(31));
        assertThat(lines).anySatisfy(l -> assertThat(l).startsWith("Account ID         :00000000001"));
        assertThat(lines).anySatisfy(l -> assertThat(l).startsWith("Total EXP:"));

        String html = read("statements/2022-07-18/s1/statement.html");
        assertThat(html.lines()).allSatisfy(l -> assertThat(l).hasSize(100));
        assertThat(html).contains("<h3>Statement for Account Number: 00000000001")
                .contains("<p style=\"font-size:16px\">Bank of XYZ</p>");

        assertThat(run("statement-pdf", "p1", "--statementRunId=s1")).isEqualTo(ReturnCode.OK);
        byte[] pdf = readBytes("statements/2022-07-18/s1/statement.pdf");
        try (PDDocument doc = Loader.loadPDF(pdf)) {
            assertThat(doc.getNumberOfPages()).isGreaterThanOrEqualTo(xrefs);
            assertThat(new PDFTextStripper().getText(doc)).contains("START OF STATEMENT");
        }
    }

    @Test
    void transactionReportForProcessingDateRange() throws IOException {
        assertThat(run("post-daily-transactions", "r1")).isEqualTo(ReturnCode.WARNING);
        assertThat(run("transaction-report", "r1", "--dateParm=2022-07-18 2022-07-18")).isEqualTo(ReturnCode.OK);
        List<String> report = read("reports/tranrept/2022-07-18/r1.txt").lines().toList();
        assertThat(report).allSatisfy(l -> assertThat(l).hasSize(133));
        assertThat(report.get(0)).contains("Daily Transaction Report")
                .contains("Date Range: 2022-07-18 to 2022-07-18");
        BigDecimal sum = jdbc.queryForObject("SELECT sum(amt) FROM transaction", BigDecimal.class);
        String grand = report.get(report.size() - 1);
        assertThat(grand).startsWith("Grand Total");
        assertThat(new BigDecimal(grand.substring(97).replace(",", "").replace("+", "").trim()))
                .isEqualByComparingTo(sum);
        long details = report.stream().filter(l -> l.length() > 16 && l.substring(0, 16).matches("\\d{16}")).count();
        assertThat(details).isEqualTo(262);

        assertThat(run("transaction-report", "r2", "--startDate=2022-01-01", "--endDate=2022-01-31"))
                .isEqualTo(ReturnCode.OK);
        assertThat(result("r2", "transaction-report").at("/counts/transactions").asInt()).isZero();
        assertThat(run("transaction-report", "r3", "--startDate=2022-02-01", "--endDate=2022-01-31"))
                .isEqualTo(ReturnCode.INPUT_ERROR);
    }

    @Test
    void weeklyTransactionTypeMaintenanceExtractAndRefresh() throws IOException {
        put("input/trantype-maint/2022-07-18/maint.txt", String.join("\n",
                "* weekly maintenance",
                "A08Chargeback",
                "U07Manual adjustment",
                "D08",
                "D09",
                "D01",
                "X10Bad action") + "\n");
        assertThat(run("maintain-transaction-types", "w1")).isEqualTo(ReturnCode.WARNING);
        JsonNode maint = result("w1", "maintain-transaction-types").get("counts");
        assertThat(maint.get("added").asInt()).isEqualTo(1);
        assertThat(maint.get("updated").asInt()).isEqualTo(1);
        assertThat(maint.get("deleted").asInt()).isEqualTo(1);
        assertThat(maint.get("comments").asInt()).isEqualTo(1);
        assertThat(maint.get("errors").asInt()).isEqualTo(3);
        assertThat(jdbc.queryForObject("SELECT description FROM transaction_type WHERE type_cd = '07'",
                String.class)).isEqualTo("Manual adjustment");

        assertThat(run("extract-transaction-types", "w1")).isEqualTo(ReturnCode.OK);
        List<String> types = lines(read("refdata/transaction_type/w1.txt"));
        assertThat(types).hasSize(7).allSatisfy(l -> assertThat(l).hasSize(60));
        assertThat(types.get(6)).startsWith("07Manual adjustment");
        assertThat(lines(read("refdata/transaction_category/w1.txt"))).hasSize(
                jdbc.queryForObject("SELECT count(*) FROM transaction_category", Integer.class));

        jdbc.update("UPDATE transaction_type SET description = 'changed' WHERE type_cd = '07'");
        assertThat(run("load-reference-data", "w1", "--table=transaction_type")).isEqualTo(ReturnCode.OK);
        assertThat(jdbc.queryForObject("SELECT description FROM transaction_type WHERE type_cd = '07'",
                String.class)).isEqualTo("Manual adjustment");

        assertThat(run("backup-reference-data", "w1", "--table=disclosure_group")).isEqualTo(ReturnCode.OK);
        assertThat(lines(gunzip(readBytes("backup/disclosure_group/2022-07-18/w1.csv.gz")))).hasSize(
                1 + jdbc.queryForObject("SELECT count(*) FROM disclosure_group", Integer.class));
    }

    @Test
    void referenceRefreshThatWouldOrphanRowsRollsBackWithRc12() throws IOException {
        put("refdata/transaction_type/only-01.txt", "01Purchase\n");
        assertThat(run("load-reference-data", "x1", "--table=transaction_type",
                "--sourceKey=refdata/transaction_type/only-01.txt")).isEqualTo(ReturnCode.DATA_ERROR);
        assertThat(jdbc.queryForObject("SELECT count(*) FROM transaction_type", Integer.class)).isEqualTo(7);
        assertThat(run("load-reference-data", "x2", "--table=nope")).isEqualTo(ReturnCode.INPUT_ERROR);
    }

    @Test
    void unknownJobIsInputError() {
        assertThat(runner.run("--job=does-not-exist")).isEqualTo(ReturnCode.INPUT_ERROR);
        assertThat(run("post-daily-transactions", "bad", "--businessDate=2022-13-01"))
                .isEqualTo(ReturnCode.INPUT_ERROR);
        assertThat(run("post-daily-transactions", "x".repeat(41))).isEqualTo(ReturnCode.INPUT_ERROR);
        assertThat(run("post-daily-transactions", "../escape")).isEqualTo(ReturnCode.INPUT_ERROR);
    }

    static String gunzip(byte[] bytes) throws IOException {
        try (GZIPInputStream in = new GZIPInputStream(new ByteArrayInputStream(bytes))) {
            return new String(in.readAllBytes(), StandardCharsets.UTF_8);
        }
    }
}
