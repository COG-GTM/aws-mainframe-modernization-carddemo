package com.carddemo.batch.source;

import com.carddemo.common.cobol.CobolValues;
import com.carddemo.common.record.CobolRecord;
import com.carddemo.common.record.CodePage;
import com.carddemo.common.record.layout.AccountRecordLayout;
import com.carddemo.common.record.layout.CardRecordLayout;
import com.carddemo.common.record.layout.CardXrefRecordLayout;
import com.carddemo.common.record.layout.CustomerRecordLayout;
import com.carddemo.common.record.layout.TranRecordLayout;
import java.math.BigDecimal;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Component;

/**
 * Read-only unload of the service tables batch-service jobs read when a KSDS DD is not given, as
 * the copybook records the COBOL reads (the REPROC unload of the cluster): {@code customer.customers}
 * (CUSTFILE), {@code account.accounts} (ACCTFILE), {@code card.cards} (CARDFILE), {@code card.card_xref}
 * (XREFFILE) and {@code transaction.transactions} (TRANSACT). Columns follow
 * {@code docs/migration/schema-mapping.md}; FILLER is blank, a NULL text or date is blank, a NULL
 * number zero. Rows come in primary-key order (text keys in byte order). batch-service never writes
 * these schemas (architecture.md).
 */
@Component
public class SourceTables {

    /** TRAN-SOURCE of the interest transactions INTCALC writes, which carry DB2 timestamps throughout. */
    static final String SYSTEM_SOURCE = "System";
    static final String INTEREST_DESCRIPTION = "Int. for a/c ";
    static final DateTimeFormatter FILE_TIMESTAMP = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSSSSS");

    private final JdbcTemplate jdbc;

    public SourceTables(JdbcTemplate jdbc) {
        this.jdbc = jdbc;
    }

    public List<CobolRecord> customers(CodePage codePage) {
        return jdbc.query("SELECT cust_id, first_name, middle_name, last_name, addr_line_1, addr_line_2, addr_line_3,"
                + " addr_state_cd, addr_country_cd, addr_zip, phone_num_1, phone_num_2, ssn, govt_issued_id,"
                + " date_of_birth, eft_account_id, pri_card_holder_ind, fico_credit_score"
                + " FROM customer.customers ORDER BY cust_id", (rs, n) -> CobolRecord.initialized(CustomerRecordLayout.LAYOUT, codePage)
                .set(CustomerRecordLayout.CUST_ID, rs.getLong(1))
                .set(CustomerRecordLayout.CUST_FIRST_NAME, rs.getString(2))
                .set(CustomerRecordLayout.CUST_MIDDLE_NAME, rs.getString(3))
                .set(CustomerRecordLayout.CUST_LAST_NAME, rs.getString(4))
                .set(CustomerRecordLayout.CUST_ADDR_LINE_1, rs.getString(5))
                .set(CustomerRecordLayout.CUST_ADDR_LINE_2, rs.getString(6))
                .set(CustomerRecordLayout.CUST_ADDR_LINE_3, rs.getString(7))
                .set(CustomerRecordLayout.CUST_ADDR_STATE_CD, rs.getString(8))
                .set(CustomerRecordLayout.CUST_ADDR_COUNTRY_CD, rs.getString(9))
                .set(CustomerRecordLayout.CUST_ADDR_ZIP, rs.getString(10))
                .set(CustomerRecordLayout.CUST_PHONE_NUM_1, rs.getString(11))
                .set(CustomerRecordLayout.CUST_PHONE_NUM_2, rs.getString(12))
                .set(CustomerRecordLayout.CUST_SSN, rs.getLong(13))
                .set(CustomerRecordLayout.CUST_GOVT_ISSUED_ID, rs.getString(14))
                .set(CustomerRecordLayout.CUST_DOB_YYYY_MM_DD, date(rs, 15))
                .set(CustomerRecordLayout.CUST_EFT_ACCOUNT_ID, rs.getString(16))
                .set(CustomerRecordLayout.CUST_PRI_CARD_HOLDER_IND, rs.getString(17))
                .set(CustomerRecordLayout.CUST_FICO_CREDIT_SCORE, rs.getLong(18)));
    }

    public List<CobolRecord> accounts(CodePage codePage) {
        return jdbc.query("SELECT acct_id, active_status, curr_bal, credit_limit, cash_credit_limit, open_date,"
                + " expiration_date, reissue_date, curr_cyc_credit, curr_cyc_debit, addr_zip, group_id"
                + " FROM account.accounts ORDER BY acct_id", (rs, n) -> CobolRecord.initialized(AccountRecordLayout.LAYOUT, codePage)
                .set(AccountRecordLayout.ACCT_ID, rs.getLong(1))
                .set(AccountRecordLayout.ACCT_ACTIVE_STATUS, rs.getString(2))
                .set(AccountRecordLayout.ACCT_CURR_BAL, amount(rs, 3))
                .set(AccountRecordLayout.ACCT_CREDIT_LIMIT, amount(rs, 4))
                .set(AccountRecordLayout.ACCT_CASH_CREDIT_LIMIT, amount(rs, 5))
                .set(AccountRecordLayout.ACCT_OPEN_DATE, date(rs, 6))
                .set(AccountRecordLayout.ACCT_EXPIRAION_DATE, date(rs, 7))
                .set(AccountRecordLayout.ACCT_REISSUE_DATE, date(rs, 8))
                .set(AccountRecordLayout.ACCT_CURR_CYC_CREDIT, amount(rs, 9))
                .set(AccountRecordLayout.ACCT_CURR_CYC_DEBIT, amount(rs, 10))
                .set(AccountRecordLayout.ACCT_ADDR_ZIP, rs.getString(11))
                .set(AccountRecordLayout.ACCT_GROUP_ID, rs.getString(12)));
    }

    public List<CobolRecord> cards(CodePage codePage) {
        return jdbc.query("SELECT card_num, acct_id, cvv_cd, embossed_name, expiration_date, active_status"
                + " FROM card.cards ORDER BY card_num COLLATE \"C\"", (rs, n) -> CobolRecord.initialized(CardRecordLayout.LAYOUT, codePage)
                .set(CardRecordLayout.CARD_NUM, rs.getString(1))
                .set(CardRecordLayout.CARD_ACCT_ID, rs.getLong(2))
                .set(CardRecordLayout.CARD_CVV_CD, rs.getLong(3))
                .set(CardRecordLayout.CARD_EMBOSSED_NAME, rs.getString(4))
                .set(CardRecordLayout.CARD_EXPIRAION_DATE, date(rs, 5))
                .set(CardRecordLayout.CARD_ACTIVE_STATUS, rs.getString(6)));
    }

    public List<CobolRecord> crossReferences(CodePage codePage) {
        return jdbc.query("SELECT card_num, cust_id, acct_id FROM card.card_xref ORDER BY card_num COLLATE \"C\"",
                (rs, n) -> CobolRecord.initialized(CardXrefRecordLayout.LAYOUT, codePage)
                        .set(CardXrefRecordLayout.XREF_CARD_NUM, rs.getString(1))
                        .set(CardXrefRecordLayout.XREF_CUST_ID, rs.getLong(2))
                        .set(CardXrefRecordLayout.XREF_ACCT_ID, rs.getLong(3)));
    }

    /**
     * TRANSACT. Posted transactions carry the DALYTRAN-ORIG-TS they were copied from (file layout) and a
     * DB2-format processing timestamp; INTCALC's ({@code source = 'System'}) carry DB2 timestamps in both,
     * as transaction-service writes them to its TRANSACT DD.
     */
    /**
     * The table keeps no provenance, so INTCALC rows are recognised by the full record CBACT04C writes (type 01,
     * category 05, source System, description "Int. for a/c <acct>"), not by the user-editable source alone.
     */
    public static boolean interestTransaction(String typeCd, long catCd, String source, String description) {
        return "01".equals(typeCd) && catCd == 5 && SYSTEM_SOURCE.equals(source)
                && description != null && description.startsWith(INTEREST_DESCRIPTION);
    }

    public List<CobolRecord> transactions(CodePage codePage) {
        return jdbc.query("SELECT tran_id, type_cd, cat_cd, source, description, amount, merchant_id, merchant_name,"
                + " merchant_city, merchant_zip, card_num, orig_ts, proc_ts"
                + " FROM transaction.transactions ORDER BY tran_id COLLATE \"C\"", (rs, n) -> {
                    boolean system = interestTransaction(rs.getString(2), rs.getLong(3), rs.getString(4), rs.getString(5));
                    LocalDateTime orig = rs.getObject(12, LocalDateTime.class);
                    LocalDateTime proc = rs.getObject(13, LocalDateTime.class);
                    return CobolRecord.initialized(TranRecordLayout.LAYOUT, codePage)
                            .set(TranRecordLayout.TRAN_ID, rs.getString(1))
                            .set(TranRecordLayout.TRAN_TYPE_CD, rs.getString(2))
                            .set(TranRecordLayout.TRAN_CAT_CD, rs.getLong(3))
                            .set(TranRecordLayout.TRAN_SOURCE, rs.getString(4))
                            .set(TranRecordLayout.TRAN_DESC, rs.getString(5))
                            .set(TranRecordLayout.TRAN_AMT, amount(rs, 6))
                            .set(TranRecordLayout.TRAN_MERCHANT_ID, rs.getLong(7))
                            .set(TranRecordLayout.TRAN_MERCHANT_NAME, rs.getString(8))
                            .set(TranRecordLayout.TRAN_MERCHANT_CITY, rs.getString(9))
                            .set(TranRecordLayout.TRAN_MERCHANT_ZIP, rs.getString(10))
                            .set(TranRecordLayout.TRAN_CARD_NUM, rs.getString(11))
                            .set(TranRecordLayout.TRAN_ORIG_TS, orig == null ? null
                                    : system ? CobolValues.toDb2Timestamp(orig) : FILE_TIMESTAMP.format(orig))
                            .set(TranRecordLayout.TRAN_PROC_TS, CobolValues.toDb2Timestamp(proc));
                });
    }

    private static String date(ResultSet rs, int column) throws SQLException {
        LocalDate value = rs.getObject(column, LocalDate.class);
        return value == null ? null : value.toString();
    }

    private static BigDecimal amount(ResultSet rs, int column) throws SQLException {
        BigDecimal value = rs.getBigDecimal(column);
        return value == null ? BigDecimal.ZERO : value;
    }
}
