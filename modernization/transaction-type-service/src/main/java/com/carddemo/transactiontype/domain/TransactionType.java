package com.carddemo.transactiontype.domain;

/** Row of CARDDEMO.TRANSACTION_TYPE (DCLTRTYP: TR_TYPE CHAR(2), TR_DESCRIPTION VARCHAR(50)). */
public record TransactionType(String code, String description) {
}
