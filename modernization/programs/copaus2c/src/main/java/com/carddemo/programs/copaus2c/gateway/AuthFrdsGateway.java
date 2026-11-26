package com.carddemo.programs.copaus2c.gateway;

import com.carddemo.programs.copaus2c.copybook.AuthFrdsRow;
import com.carddemo.programs.copaus2c.copybook.Sqlca;

/**
 * DB2 access used by COPAUS2C against CARDDEMO.AUTHFRDS. Implementations must report the outcome as an
 * SQLCA (SQLCODE/SQLSTATE) exactly as DB2 would, never by throwing.
 */
public interface AuthFrdsGateway {

    /**
     * {@code INSERT INTO CARDDEMO.AUTHFRDS (...) VALUES (..., TIMESTAMP_FORMAT(:AUTH-TS,
     * 'YY-MM-DD HH24.MI.SSNNNNNN'), ..., CURRENT DATE, :ACCT-ID, :CUST-ID)}.
     * FRAUD_RPT_DATE is set to CURRENT DATE; the row's FRAUD-RPT-DATE host variable is not bound.
     */
    Sqlca insert(AuthFrdsRow row);

    /**
     * {@code UPDATE CARDDEMO.AUTHFRDS SET AUTH_FRAUD = :AUTH-FRAUD, FRAUD_RPT_DATE = CURRENT DATE
     * WHERE CARD_NUM = :CARD-NUM AND AUTH_TS = TIMESTAMP_FORMAT(:AUTH-TS, 'YY-MM-DD HH24.MI.SSNNNNNN')}.
     */
    Sqlca updateFraud(String authFraud, String cardNum, String authTs);
}
