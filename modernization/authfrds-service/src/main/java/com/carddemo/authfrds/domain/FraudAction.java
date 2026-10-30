package com.carddemo.authfrds.domain;

/** WS-FRD-ACTION in the COPAUS2C commarea (88 WS-REPORT-FRAUD 'F' / WS-REMOVE-FRAUD 'R'). */
public enum FraudAction {
    REPORT("F"), REMOVE("R");

    private final String code;

    FraudAction(String code) {
        this.code = code;
    }

    public String code() {
        return code;
    }

    public static FraudAction fromCode(String code) {
        for (FraudAction a : values()) {
            if (a.code.equals(code)) {
                return a;
            }
        }
        throw new IllegalArgumentException("WS-FRD-ACTION must be F or R");
    }
}
