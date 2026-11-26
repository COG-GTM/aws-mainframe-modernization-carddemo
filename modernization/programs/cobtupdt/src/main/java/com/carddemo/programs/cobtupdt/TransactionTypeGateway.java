package com.carddemo.programs.cobtupdt;

/**
 * The three static SQL statements COBTUPDT issues against CARDDEMO.TRANSACTION_TYPE.
 * Each method returns the resulting SQLCODE (0 = OK, +100 = no row, &lt; 0 = error).
 * Host variables are passed exactly as the COBOL program binds them:
 * {@code :INPUT-REC-NUMBER} (PIC X(2)) and {@code :INPUT-REC-DESC} (PIC X(50), blank padded).
 */
public interface TransactionTypeGateway {

    /** INSERT INTO CARDDEMO.TRANSACTION_TYPE (TR_TYPE, TR_DESCRIPTION) VALUES (:INPUT-REC-NUMBER, :INPUT-REC-DESC). */
    int insert(String trType, String trDescription);

    /** UPDATE CARDDEMO.TRANSACTION_TYPE SET TR_DESCRIPTION = :INPUT-REC-DESC WHERE TR_TYPE = :INPUT-REC-NUMBER. */
    int update(String trType, String trDescription);

    /** DELETE FROM CARDDEMO.TRANSACTION_TYPE WHERE TR_TYPE = :INPUT-REC-NUMBER. */
    int delete(String trType);
}
