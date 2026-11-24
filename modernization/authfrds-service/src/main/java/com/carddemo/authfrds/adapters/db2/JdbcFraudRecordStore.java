package com.carddemo.authfrds.adapters.db2;

import com.carddemo.authfrds.domain.AuthFraudRules;
import com.carddemo.authfrds.domain.FraudFilter;
import com.carddemo.authfrds.domain.FraudRecord;
import com.carddemo.authfrds.domain.FraudRecordStore;
import com.carddemo.authfrds.domain.FraudRow;
import com.carddemo.authfrds.domain.SqlCodeException;
import com.carddemo.authfrds.domain.SqlCodes;
import java.sql.Date;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.Supplier;
import org.springframework.dao.DataAccessException;
import org.springframework.dao.DataAccessResourceFailureException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.stereotype.Repository;

/** Db2 (IBM JCC) adapter. INSERT / UPDATE are the embedded SQL of COPAUS2C, column for column. */
@Repository
public class JdbcFraudRecordStore implements FraudRecordStore {

    private static final String COLUMNS = "CARD_NUM, AUTH_TS, AUTH_TYPE, CARD_EXPIRY_DATE, MESSAGE_TYPE, "
            + "MESSAGE_SOURCE, AUTH_ID_CODE, AUTH_RESP_CODE, AUTH_RESP_REASON, PROCESSING_CODE, TRANSACTION_AMT, "
            + "APPROVED_AMT, MERCHANT_CATAGORY_CODE, ACQR_COUNTRY_CODE, POS_ENTRY_MODE, MERCHANT_ID, MERCHANT_NAME, "
            + "MERCHANT_CITY, MERCHANT_STATE, MERCHANT_ZIP, TRANSACTION_ID, MATCH_STATUS, AUTH_FRAUD, "
            + "FRAUD_RPT_DATE, ACCT_ID, CUST_ID";
    private static final String TS = "TIMESTAMP_FORMAT(?, '" + AuthFraudRules.DB2_TIMESTAMP_FORMAT + "')";

    private static final RowMapper<FraudRecord> ROW = (rs, i) -> {
        Timestamp ts = rs.getTimestamp("AUTH_TS");
        Date rpt = rs.getDate("FRAUD_RPT_DATE");
        return new FraudRecord(rs.getString("CARD_NUM"), ts == null ? null : ts.toLocalDateTime(),
                rs.getString("AUTH_TYPE"), rs.getString("CARD_EXPIRY_DATE"), rs.getString("MESSAGE_TYPE"),
                rs.getString("MESSAGE_SOURCE"), rs.getString("AUTH_ID_CODE"), rs.getString("AUTH_RESP_CODE"),
                rs.getString("AUTH_RESP_REASON"), rs.getString("PROCESSING_CODE"), rs.getBigDecimal("TRANSACTION_AMT"),
                rs.getBigDecimal("APPROVED_AMT"), rs.getString("MERCHANT_CATAGORY_CODE"),
                rs.getString("ACQR_COUNTRY_CODE"), (Integer) rs.getObject("POS_ENTRY_MODE", Integer.class),
                rs.getString("MERCHANT_ID"), rs.getString("MERCHANT_NAME"), rs.getString("MERCHANT_CITY"),
                rs.getString("MERCHANT_STATE"), rs.getString("MERCHANT_ZIP"), rs.getString("TRANSACTION_ID"),
                rs.getString("MATCH_STATUS"), rs.getString("AUTH_FRAUD"), rpt == null ? null : rpt.toLocalDate(),
                rs.getObject("ACCT_ID") == null ? null : rs.getLong("ACCT_ID"),
                rs.getObject("CUST_ID") == null ? null : rs.getLong("CUST_ID"));
    };

    private final JdbcTemplate jdbc;

    public JdbcFraudRecordStore(JdbcTemplate jdbc) {
        this.jdbc = jdbc;
    }

    @Override
    public void insert(FraudRow r) {
        // BR-AF-001 TIMESTAMP_FORMAT(:AUTH-TS, 'YY-MM-DD HH24.MI.SSNNNNNN'); BR-AF-003 FRAUD_RPT_DATE = CURRENT DATE
        String q = "INSERT INTO CARDDEMO.AUTHFRDS (" + COLUMNS + ") VALUES (?, " + TS
                + ", ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, CURRENT DATE, ?, ?)";
        sql(() -> jdbc.update(q, r.cardNum(), r.authTsHost(), r.authType(), r.cardExpiryDate(), r.messageType(),
                r.messageSource(), r.authIdCode(), r.authRespCode(), r.authRespReason(), r.processingCode(),
                r.transactionAmt(), r.approvedAmt(), r.merchantCategoryCode(), r.acqrCountryCode(), r.posEntryMode(),
                r.merchantId(), r.merchantName(), r.merchantCity(), r.merchantState(), r.merchantZip(),
                r.transactionId(), r.matchStatus(), r.authFraud(), r.acctId(), r.custId()));
    }

    @Override
    public void updateFraudFlag(String cardNum, String authTsHost, String authFraud) {
        // BR-AF-005 FRAUD-UPDATE: only AUTH_FRAUD and FRAUD_RPT_DATE (BR-AF-003) change
        int n = sql(() -> jdbc.update("UPDATE CARDDEMO.AUTHFRDS SET AUTH_FRAUD = ?, FRAUD_RPT_DATE = CURRENT DATE "
                + "WHERE CARD_NUM = ? AND AUTH_TS = " + TS, authFraud, cardNum, authTsHost));
        if (n == 0) {
            // BR-AF-007: a searched UPDATE that hits no row is SQLCODE +100 / SQLSTATE 02000 in the SQLCA
            throw new SqlCodeException(SqlCodes.NOT_FOUND, "02000", "row not found for UPDATE", null);
        }
    }

    @Override
    public Optional<FraudRecord> findByHostTs(String cardNum, String authTsHost) {
        return sql(() -> jdbc.query("SELECT " + COLUMNS + " FROM CARDDEMO.AUTHFRDS WHERE CARD_NUM = ? AND AUTH_TS = "
                + TS, ROW, cardNum, authTsHost).stream().findFirst());
    }

    @Override
    public Optional<FraudRecord> find(String cardNum, LocalDateTime authTs) {
        return sql(() -> jdbc.query("SELECT " + COLUMNS + " FROM CARDDEMO.AUTHFRDS WHERE CARD_NUM = ? AND AUTH_TS = ?",
                ROW, cardNum, Timestamp.valueOf(authTs)).stream().findFirst());
    }

    /** BR-AF-030: inspection read in XAUTHFRD order (CARD_NUM ASC, AUTH_TS DESC). Not in the legacy programs. */
    @Override
    public List<FraudRecord> list(FraudFilter filter, int offset, int limit) {
        List<Object> args = new ArrayList<>();
        String q = "SELECT " + COLUMNS + " FROM CARDDEMO.AUTHFRDS WHERE 1 = 1" + where(filter, args)
                + " ORDER BY CARD_NUM ASC, AUTH_TS DESC OFFSET " + offset + " ROWS FETCH FIRST " + limit + " ROWS ONLY";
        return sql(() -> jdbc.query(q, ROW, args.toArray()));
    }

    @Override
    public long count(FraudFilter filter) {
        List<Object> args = new ArrayList<>();
        String q = "SELECT COUNT(1) FROM CARDDEMO.AUTHFRDS WHERE 1 = 1" + where(filter, args);
        return sql(() -> jdbc.queryForObject(q, Long.class, args.toArray()));
    }

    private static String where(FraudFilter f, List<Object> args) {
        StringBuilder sb = new StringBuilder();
        if (f.cardNum() != null) {
            sb.append(" AND CARD_NUM = ?");
            args.add(f.cardNum());
        }
        if (f.acctId() != null) {
            sb.append(" AND ACCT_ID = ?");
            args.add(f.acctId());
        }
        if (f.authFraud() != null) {
            sb.append(" AND AUTH_FRAUD = ?");
            args.add(f.authFraud());
        }
        return sb.toString();
    }

    /** Surfaces the real SQLCODE / SQLSTATE (SQLCA) from the JCC driver. */
    private static <T> T sql(Supplier<T> call) {
        try {
            return call.get();
        } catch (DataAccessResourceFailureException e) {
            throw e;
        } catch (DataAccessException e) {
            SQLException se = findSqlException(e);
            if (se == null) {
                throw e;
            }
            throw new SqlCodeException(se.getErrorCode(), se.getSQLState(), se.getMessage(), e);
        }
    }

    private static SQLException findSqlException(Throwable t) {
        while (t != null) {
            if (t instanceof SQLException se) {
                return se;
            }
            t = t.getCause();
        }
        return null;
    }
}
