package com.carddemo.authfrds.domain;

/**
 * COPAUS2C WS-FRAUD-STATUS-RECORD returned to the caller: WS-FRD-UPDATE-STATUS ('S'/'F') and WS-FRD-ACT-MSG,
 * plus the SQLCODE/SQLSTATE and the commarea's PA-FRAUD-RPT-DATE.
 */
public record FraudResult(String updateStatus, String operation, String message, Integer sqlCode, String sqlState,
                          String authTs, String fraudRptDate, FraudRecord record) {

    public boolean success() {
        return "S".equals(updateStatus);
    }
}
