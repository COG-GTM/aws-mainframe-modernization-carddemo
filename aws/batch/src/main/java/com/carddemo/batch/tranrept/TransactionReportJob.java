package com.carddemo.batch.tranrept;

import com.carddemo.batch.core.CardDemoJob;
import com.carddemo.batch.core.JobFailure;
import com.carddemo.batch.core.JobOutcome;
import com.carddemo.batch.core.JobParams;
import com.carddemo.batch.core.ReturnCode;
import com.carddemo.batch.record.Fixed;
import com.carddemo.batch.record.Zoned;
import com.carddemo.batch.storage.ObjectStore;
import com.carddemo.batch.storage.S3Keys;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.sql.Date;
import java.time.LocalDate;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Component;

/**
 * {@code TRANREPT.jcl} (REPROC unload → SORT {@code INCLUDE} on {@code TRAN-PROC-DT} → {@code CBTRN03C}):
 * 133-byte "Daily Transaction Report" for {@code proc_ts::date BETWEEN startDate AND endDate}, grouped by card
 * number, page size 20, with account, page and grand totals ({@link ReportFormatter}).
 * Parameters: {@code startDate}/{@code endDate} ({@code yyyy-MM-dd}) or legacy {@code dateParm="yyyy-mm-dd yyyy-mm-dd"};
 * both default to {@code businessDate}.
 */
@Component
public class TransactionReportJob implements CardDemoJob {

    private final JdbcTemplate jdbc;
    private final ObjectStore store;

    public TransactionReportJob(JdbcTemplate jdbc, ObjectStore store) {
        this.jdbc = jdbc;
        this.store = store;
    }

    @Override
    public String name() {
        return "transaction-report";
    }

    @Override
    public JobOutcome run(JobParams p) {
        LocalDate start;
        LocalDate end;
        if (p.get("dateParm").isPresent()) {
            String parm = Fixed.pad(p.require("dateParm"), 21);
            start = LocalDate.parse(parm.substring(0, 10));
            end = LocalDate.parse(parm.substring(11, 21));
        } else {
            start = p.get("startDate").map(LocalDate::parse).orElse(p.businessDate());
            end = p.get("endDate").map(LocalDate::parse).orElse(p.businessDate());
        }
        if (end.isBefore(start)) {
            throw new JobFailure(ReturnCode.INPUT_ERROR, "endDate " + end + " is before startDate " + start);
        }
        List<ReportFormatter.Detail> details = jdbc.query("""
                SELECT t.tran_id, t.card_num, x.acct_id, t.type_cd, tt.description AS type_desc, t.cat_cd,
                       tc.description AS cat_desc, t.source, t.amt
                  FROM transaction t
                  LEFT JOIN card_xref x ON x.card_num = t.card_num
                  LEFT JOIN transaction_type tt ON tt.type_cd = t.type_cd
                  LEFT JOIN transaction_category tc ON tc.type_cd = t.type_cd AND tc.cat_cd = t.cat_cd
                 WHERE CAST(t.proc_ts AS DATE) BETWEEN ? AND ?
                 ORDER BY t.card_num, t.tran_id
                """, (rs, i) -> {
                    Object acct = rs.getObject("acct_id");
                    if (acct == null || rs.getString("type_desc") == null || rs.getString("cat_desc") == null) {
                        throw new JobFailure(ReturnCode.DATA_ERROR, "Lookup failed for transaction "
                                + rs.getString("tran_id") + " (card xref / type / category not found)");
                    }
                    return new ReportFormatter.Detail(rs.getString("tran_id"), rs.getString("card_num"),
                            Zoned.formatUnsigned(rs.getLong("acct_id"), 11), rs.getString("type_cd"),
                            rs.getString("type_desc"), rs.getInt("cat_cd"), rs.getString("cat_desc"),
                            rs.getString("source"), rs.getBigDecimal("amt"));
                }, Date.valueOf(start), Date.valueOf(end));

        ReportFormatter.Report report = ReportFormatter.format(start, end, details);
        String key = S3Keys.transactionReport(p.businessDate(), p.runId());
        store.put(key, report.text().getBytes(StandardCharsets.US_ASCII), "text/plain");
        Map<String, Object> counts = new LinkedHashMap<>();
        counts.put("startDate", start.toString());
        counts.put("endDate", end.toString());
        counts.put("transactions", details.size());
        counts.put("lines", report.lines());
        counts.put("grandTotal", report.grandTotal().toPlainString());
        counts.put("output", store.uri(key));
        return JobOutcome.ok(counts);
    }

}
