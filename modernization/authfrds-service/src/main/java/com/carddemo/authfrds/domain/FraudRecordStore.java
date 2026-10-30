package com.carddemo.authfrds.domain;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

/** Port for CARDDEMO.AUTHFRDS. Write methods raise {@link SqlCodeException} with the real SQLCODE. */
public interface FraudRecordStore {

    /** COPAUS2C MAIN-PARA: INSERT ... VALUES (..., TIMESTAMP_FORMAT(:AUTH-TS, ...), ..., CURRENT DATE, ...). */
    void insert(FraudRow row);

    /** COPAUS2C FRAUD-UPDATE: UPDATE ... SET AUTH_FRAUD, FRAUD_RPT_DATE = CURRENT DATE WHERE key. +100 if no row. */
    void updateFraudFlag(String cardNum, String authTsHost, String authFraud);

    Optional<FraudRecord> findByHostTs(String cardNum, String authTsHost);

    Optional<FraudRecord> find(String cardNum, LocalDateTime authTs);

    List<FraudRecord> list(FraudFilter filter, int offset, int limit);

    long count(FraudFilter filter);
}
