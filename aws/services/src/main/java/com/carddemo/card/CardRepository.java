package com.carddemo.card;

import com.carddemo.common.KeysetPager;
import com.carddemo.common.PageQuery;
import com.carddemo.common.PageResponse;
import java.sql.Date;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.jdbc.core.simple.JdbcClient;
import org.springframework.stereotype.Repository;

/** CARDDAT. */
@Repository
public class CardRepository {

    static final RowMapper<CardRecord> MAPPER = (rs, n) -> {
        Date exp = rs.getDate("expiration_date");
        return new CardRecord(rs.getString("card_num"), rs.getLong("acct_id"), rs.getInt("cvv_cd"),
                rs.getString("embossed_name"), exp == null ? null : exp.toLocalDate(), rs.getString("active_status"),
                rs.getLong("version"));
    };

    private final JdbcClient jdbc;
    private final KeysetPager pager;

    public CardRepository(JdbcClient jdbc, KeysetPager pager) {
        this.jdbc = jdbc;
        this.pager = pager;
    }

    public PageResponse<CardRecord> page(Long acctId, String cardNum, PageQuery query) {
        List<String> filters = new ArrayList<>();
        Map<String, Object> params = new HashMap<>();
        if (acctId != null) {
            filters.add("acct_id = :acctId");
            params.put("acctId", acctId);
        }
        if (cardNum != null) {
            filters.add("card_num = :cardNum");
            params.put("cardNum", cardNum);
        }
        return pager.page("SELECT * FROM card", "card_num", String.join(" AND ", filters), params, query, MAPPER,
                CardRecord::cardNum);
    }

    public Optional<CardRecord> findById(String cardNum) {
        return jdbc.sql("SELECT * FROM card WHERE card_num = :id").param("id", cardNum).query(MAPPER).optional();
    }

    public Optional<CardRecord> lockNoWait(String cardNum) {
        return jdbc.sql("SELECT * FROM card WHERE card_num = :id FOR UPDATE NOWAIT").param("id", cardNum)
                .query(MAPPER).optional();
    }

    public int update(CardRecord card) {
        return jdbc.sql("""
                UPDATE card SET embossed_name = :name, active_status = :status, expiration_date = :exp,
                       version = version + 1
                 WHERE card_num = :id AND version = :version""")
                .param("name", card.embossedName()).param("status", card.activeStatus())
                .param("exp", card.expirationDate()).param("id", card.cardNum()).param("version", card.version())
                .update();
    }
}
