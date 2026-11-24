package com.carddemo.transactiontype.domain;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.TreeMap;
import java.util.regex.Pattern;

/** Test double that behaves like the Db2 tables, including the SQLCODEs the COBOL branches on. */
public class InMemoryTransactionTypeStore implements TransactionTypeStore {

    public final TreeMap<String, String> types = new TreeMap<>();
    public final List<TransactionTypeCategory> categories = new ArrayList<>();
    private Integer failNextMutationWith;

    public static InMemoryTransactionTypeStore seeded() {
        var s = new InMemoryTransactionTypeStore();
        Map.of("01", "PURCHASE", "02", "PAYMENT", "03", "CREDIT", "04", "AUTHORIZATION",
                "05", "REFUND", "06", "REVERAL", "07", "ADJUSTMENT").forEach(s.types::put);
        s.categories.add(new TransactionTypeCategory("01", "0001", "REGULAR SALES DRAFT"));
        return s;
    }

    public void failNextMutationWith(int sqlCode) {
        failNextMutationWith = sqlCode;
    }

    private void maybeFail() {
        if (failNextMutationWith != null) {
            int code = failNextMutationWith;
            failNextMutationWith = null;
            throw new SqlCodeException(code, "simulated SQLCODE " + code, null);
        }
    }

    private boolean matches(String code, String desc, ListFilter f) {
        if (f.typeCode() != null && !f.typeCode().equals(code)) {
            return false;
        }
        if (f.descriptionPattern() != null) {
            String regex = Pattern.quote(f.descriptionPattern()).replace("%", "\\E.*\\Q").replace("_", "\\E.\\Q");
            return desc.matches(regex);
        }
        return true;
    }

    @Override
    public Optional<TransactionType> findByCode(String code) {
        return Optional.ofNullable(types.get(code)).map(d -> new TransactionType(code, d));
    }

    @Override
    public List<TransactionType> findForward(String startKey, ListFilter filter, int limit) {
        return types.tailMap(startKey, true).entrySet().stream()
                .filter(e -> matches(e.getKey(), e.getValue(), filter)).limit(limit)
                .map(e -> new TransactionType(e.getKey(), e.getValue())).toList();
    }

    @Override
    public List<TransactionType> findBackward(String beforeKey, ListFilter filter, int limit) {
        return types.headMap(beforeKey, false).descendingMap().entrySet().stream()
                .filter(e -> matches(e.getKey(), e.getValue(), filter)).limit(limit)
                .map(e -> new TransactionType(e.getKey(), e.getValue())).toList();
    }

    @Override
    public long count(ListFilter filter) {
        return types.entrySet().stream().filter(e -> matches(e.getKey(), e.getValue(), filter)).count();
    }

    @Override
    public void insert(TransactionType tt) {
        maybeFail();
        if (types.containsKey(tt.code())) {
            throw new SqlCodeException(SqlCodes.DUPLICATE_KEY, "duplicate", null);
        }
        types.put(tt.code(), tt.description());
    }

    @Override
    public int updateDescription(String code, String description) {
        maybeFail();
        return types.replace(code, description) == null ? 0 : 1;
    }

    @Override
    public int delete(String code) {
        maybeFail();
        if (categories.stream().anyMatch(c -> c.typeCode().equals(code))) {
            throw new SqlCodeException(SqlCodes.DELETE_RESTRICTED_BY_FK, "restrict", null);
        }
        return types.remove(code) == null ? 0 : 1;
    }

    @Override
    public List<TransactionTypeCategory> findCategories(String typeCode) {
        return categories.stream().filter(c -> c.typeCode().equals(typeCode))
                .sorted(Comparator.comparing(TransactionTypeCategory::categoryCode)).toList();
    }

    @Override
    public List<String> extractTransactionTypes() {
        return types.entrySet().stream()
                .map(e -> e.getKey() + String.format("%-50s", e.getValue()) + "00000000").toList();
    }

    @Override
    public List<String> extractCategories() {
        return categories.stream()
                .map(c -> c.typeCode() + c.categoryCode() + String.format("%-50s", c.description()) + "0000").toList();
    }
}
