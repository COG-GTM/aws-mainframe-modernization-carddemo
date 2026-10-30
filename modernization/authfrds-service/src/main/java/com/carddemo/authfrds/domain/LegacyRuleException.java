package com.carddemo.authfrds.domain;

/** A business-rule outcome from the specs, carrying the stable rule ID (BR-AF-nnn) that produced it. */
public class LegacyRuleException extends RuntimeException {

    public enum Kind { VALIDATION, NOT_FOUND, END_OF_DATA, FRAUD_UPDATE_FAILED, IMS_ERROR }

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
