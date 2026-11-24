package com.carddemo.authfrds.domain;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/** Test fake for AUTHFRDS: enforces the (CARD_NUM, AUTH_TS) key and can inject SQLCODEs. */
public class InMemoryFraudRecordStore implements FraudRecordStore {

    private static final DateTimeFormatter TS = DateTimeFormatter.ofPattern("yy-MM-dd HH.mm.ssSSSSSS");
    final Map<String, FraudRecord> rows = new LinkedHashMap<>();
    Integer failInsert;
    String failInsertState;
    Integer failUpdate;
    LocalDate currentDate = LocalDate.of(2026, 10, 29);

    static LocalDateTime ts(String host) {
        LocalDateTime t = LocalDateTime.parse(host.trim(), TS);
        return t.withYear(2000 + t.getYear() % 100);
    }

    private static String key(String card, LocalDateTime ts) {
        return card + "|" + ts;
    }

    @Override
    public void insert(FraudRow r) {
        if (failInsert != null) {
            throw new SqlCodeException(failInsert, failInsertState, "injected", null);
        }
        LocalDateTime t = ts(r.authTsHost());
        if (rows.containsKey(key(r.cardNum(), t))) {
            throw new SqlCodeException(SqlCodes.DUPLICATE_KEY, "23505", "duplicate", null);
        }
        rows.put(key(r.cardNum(), t), new FraudRecord(r.cardNum(), t, r.authType(), r.cardExpiryDate(),
                r.messageType(), r.messageSource(), r.authIdCode(), r.authRespCode(), r.authRespReason(),
                r.processingCode(), r.transactionAmt(), r.approvedAmt(), r.merchantCategoryCode(),
                r.acqrCountryCode(), r.posEntryMode(), r.merchantId(), r.merchantName(), r.merchantCity(),
                r.merchantState(), r.merchantZip(), r.transactionId(), r.matchStatus(), r.authFraud(), currentDate,
                r.acctId(), r.custId()));
    }

    @Override
    public void updateFraudFlag(String cardNum, String authTsHost, String authFraud) {
        if (failUpdate != null) {
            throw new SqlCodeException(failUpdate, failUpdate == 100 ? "02000" : "57033", "injected", null);
        }
        String k = key(cardNum, ts(authTsHost));
        FraudRecord o = rows.get(k);
        if (o == null) {
            throw new SqlCodeException(SqlCodes.NOT_FOUND, "02000", "not found", null);
        }
        rows.put(k, new FraudRecord(o.cardNum(), o.authTs(), o.authType(), o.cardExpiryDate(), o.messageType(),
                o.messageSource(), o.authIdCode(), o.authRespCode(), o.authRespReason(), o.processingCode(),
                o.transactionAmt(), o.approvedAmt(), o.merchantCategoryCode(), o.acqrCountryCode(), o.posEntryMode(),
                o.merchantId(), o.merchantName(), o.merchantCity(), o.merchantState(), o.merchantZip(),
                o.transactionId(), o.matchStatus(), authFraud, currentDate, o.acctId(), o.custId()));
    }

    @Override
    public Optional<FraudRecord> findByHostTs(String cardNum, String authTsHost) {
        return find(cardNum, ts(authTsHost));
    }

    @Override
    public Optional<FraudRecord> find(String cardNum, LocalDateTime authTs) {
        return Optional.ofNullable(rows.get(key(cardNum, authTs)));
    }

    private List<FraudRecord> filtered(FraudFilter f) {
        List<FraudRecord> l = new ArrayList<>(rows.values().stream()
                .filter(r -> f.cardNum() == null || f.cardNum().equals(r.cardNum()))
                .filter(r -> f.acctId() == null || f.acctId().equals(r.acctId()))
                .filter(r -> f.authFraud() == null || f.authFraud().equals(r.authFraud())).toList());
        l.sort(Comparator.comparing(FraudRecord::cardNum).thenComparing(FraudRecord::authTs, Comparator.reverseOrder()));
        return l;
    }

    @Override
    public List<FraudRecord> list(FraudFilter filter, int offset, int limit) {
        List<FraudRecord> l = filtered(filter);
        return l.subList(Math.min(offset, l.size()), Math.min(offset + limit, l.size()));
    }

    @Override
    public long count(FraudFilter filter) {
        return filtered(filter).size();
    }
}
