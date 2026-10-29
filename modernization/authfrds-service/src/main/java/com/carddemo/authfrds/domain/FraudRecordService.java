package com.carddemo.authfrds.domain;

import java.time.Clock;
import java.time.LocalDate;
import org.springframework.stereotype.Service;

/** COPAUS2C - mark / unmark an authorization as fraud in CARDDEMO.AUTHFRDS. */
@Service
public class FraudRecordService {

    private final FraudRecordStore store;
    private final Clock clock;

    public FraudRecordService(FraudRecordStore store, Clock clock) {
        this.store = store;
        this.clock = clock;
    }

    public FraudResult record(FraudRequest req) {
        // BR-AF-010: EXEC CICS ASKTIME / FORMATTIME MMDDYY DATESEP -> PA-FRAUD-RPT-DATE
        String rptDate = AuthFraudRules.fraudRptDate(LocalDate.now(clock));
        FraudRow row = AuthFraudRules.toRow(req);
        try {
            store.insert(row);
            // BR-AF-004: SQLCODE = 0 -> WS-FRD-UPDT-SUCCESS, 'ADD SUCCESS'
            return ok("ADD", AuthFraudRules.ADD_SUCCESS, row, rptDate);
        } catch (SqlCodeException e) {
            if (e.sqlCode() == SqlCodes.DUPLICATE_KEY) {
                // BR-AF-005: SQLCODE = -803 -> PERFORM FRAUD-UPDATE
                return fraudUpdate(row, rptDate);
            }
            // BR-AF-008: any other SQLCODE -> WS-FRD-UPDT-FAILED, ' SYSTEM ERROR DB2: CODE:..., STATE: ...'
            return new FraudResult("F", "ADD", AuthFraudRules.systemErrorMessage(e.sqlCode(), e.sqlState()),
                    e.sqlCode(), e.sqlState(), row.authTsHost().trim(), rptDate, null);
        }
    }

    /** FRAUD-UPDATE paragraph: only AUTH_FRAUD and FRAUD_RPT_DATE change (BR-AF-005, BR-AF-003). */
    private FraudResult fraudUpdate(FraudRow row, String rptDate) {
        try {
            store.updateFraudFlag(row.cardNum(), row.authTsHost(), row.authFraud());
            // BR-AF-006: SQLCODE = 0 -> 'UPDT SUCCESS'
            return ok("UPDT", AuthFraudRules.UPDT_SUCCESS, row, rptDate);
        } catch (SqlCodeException e) {
            // BR-AF-007: any non-zero SQLCODE (incl. +100) -> ' UPDT ERROR DB2: CODE:..., STATE: ...'
            return new FraudResult("F", "UPDT", AuthFraudRules.updateErrorMessage(e.sqlCode(), e.sqlState()),
                    e.sqlCode(), e.sqlState(), row.authTsHost().trim(), rptDate, null);
        }
    }

    private FraudResult ok(String op, String msg, FraudRow row, String rptDate) {
        FraudRecord stored = store.findByHostTs(row.cardNum(), row.authTsHost()).orElse(null);
        return new FraudResult("S", op, msg, SqlCodes.OK, "00000", row.authTsHost().trim(), rptDate, stored);
    }
}
