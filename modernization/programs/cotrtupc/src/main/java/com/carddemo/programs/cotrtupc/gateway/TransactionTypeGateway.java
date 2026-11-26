package com.carddemo.programs.cotrtupc.gateway;

import com.carddemo.programs.cotrtupc.copybook.DclTransactionType;
import com.carddemo.programs.cotrtupc.copybook.Sqlca;

/**
 * Embedded SQL against CARDDEMO.TRANSACTION_TYPE. Each method mirrors one EXEC SQL statement of
 * COTRTUPC and returns the resulting SQLCA; host variables are read from / written to {@code dcl}.
 */
public interface TransactionTypeGateway {

    /** SELECT TR_TYPE, TR_DESCRIPTION INTO :DCL-TR-TYPE, :DCL-TR-DESCRIPTION WHERE TR_TYPE = :DCL-TR-TYPE. */
    Sqlca select(DclTransactionType dcl);

    /** UPDATE ... SET TR_DESCRIPTION = :DCL-TR-DESCRIPTION WHERE TR_TYPE = :DCL-TR-TYPE. */
    Sqlca update(DclTransactionType dcl);

    /** INSERT INTO ... (TR_TYPE, TR_DESCRIPTION) VALUES (:DCL-TR-TYPE, :DCL-TR-DESCRIPTION). */
    Sqlca insert(DclTransactionType dcl);

    /** DELETE FROM ... WHERE TR_TYPE = :DCL-TR-TYPE. */
    Sqlca delete(DclTransactionType dcl);
}
