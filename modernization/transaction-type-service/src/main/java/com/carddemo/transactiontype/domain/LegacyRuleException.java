package com.carddemo.transactiontype.domain;

/** A business-rule outcome from the specs, carrying the stable rule ID (BR-TT-nnn) that produced it. */
public class LegacyRuleException extends RuntimeException {

    public enum Kind { VALIDATION, NO_CHANGE, NOT_FOUND, CONFLICT, LOCKED, CONFIRMATION_REQUIRED, DB_ERROR }

    private final String ruleId;
    private final Kind kind;
    private final Integer sqlCode;

    public LegacyRuleException(String ruleId, Kind kind, String message) {
        this(ruleId, kind, message, null);
    }

    public LegacyRuleException(String ruleId, Kind kind, String message, Integer sqlCode) {
        super(message);
        this.ruleId = ruleId;
        this.kind = kind;
        this.sqlCode = sqlCode;
    }

    public String ruleId() {
        return ruleId;
    }

    public Kind kind() {
        return kind;
    }

    public Integer sqlCode() {
        return sqlCode;
    }
}
