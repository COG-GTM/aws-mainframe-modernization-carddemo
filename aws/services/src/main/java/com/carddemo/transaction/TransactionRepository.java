package com.carddemo.transaction;

import com.carddemo.common.KeysetPager;
import com.carddemo.common.PageQuery;
import com.carddemo.common.PageResponse;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.Map;
import java.util.Optional;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.jdbc.core.simple.JdbcClient;
import org.springframework.stereotype.Repository;

/** TRANSACT. */
@Repository
public class TransactionRepository {

    static final RowMapper<TransactionRecord> MAPPER = (rs, n) -> new TransactionRecord(
            rs.getString("tran_id"), rs.getString("type_cd"), rs.getInt("cat_cd"), rs.getString("source"),
            rs.getString("description"), rs.getBigDecimal("amt"), (Integer) rs.getObject("merchant_id", Integer.class),
            rs.getString("merchant_name"), rs.getString("merchant_city"), rs.getString("merchant_zip"),
            rs.getString("card_num"), ts(rs.getTimestamp("orig_ts")), ts(rs.getTimestamp("proc_ts")));

    private final JdbcClient jdbc;
    private final KeysetPager pager;

    public TransactionRepository(JdbcClient jdbc, KeysetPager pager) {
        this.jdbc = jdbc;
        this.pager = pager;
    }

    public PageResponse<TransactionRecord> page(PageQuery query) {
        return pager.page("SELECT * FROM transaction", "tran_id", null, Map.of(), query, MAPPER,
                TransactionRecord::tranId);
    }

    public Optional<TransactionRecord> findById(String tranId) {
        return jdbc.sql("SELECT * FROM transaction WHERE tran_id = :id").param("id", tranId).query(MAPPER)
                .optional();
    }

    public boolean categoryExists(String typeCd, int catCd) {
        return jdbc.sql("SELECT EXISTS (SELECT 1 FROM transaction_category WHERE type_cd = :t AND cat_cd = :c)")
                .param("t", typeCd).param("c", catCd).query(Boolean.class).single();
    }

    public void insert(TransactionRecord t) {
        jdbc.sql("""
                INSERT INTO transaction (tran_id, type_cd, cat_cd, source, description, amt, merchant_id,
                       merchant_name, merchant_city, merchant_zip, card_num, orig_ts, proc_ts)
                VALUES (:id, :type, :cat, :source, :desc, :amt, :mid, :mname, :mcity, :mzip, :card, :orig, :proc)""")
                .param("id", t.tranId()).param("type", t.typeCd()).param("cat", t.catCd())
                .param("source", t.source()).param("desc", t.description()).param("amt", t.amt())
                .param("mid", t.merchantId()).param("mname", t.merchantName()).param("mcity", t.merchantCity())
                .param("mzip", t.merchantZip()).param("card", t.cardNum()).param("orig", t.origTs())
                .param("proc", t.procTs())
                .update();
    }

    private static LocalDateTime ts(Timestamp t) {
        return t == null ? null : t.toLocalDateTime();
    }
}
