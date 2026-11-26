package com.carddemo.programs.cotrtlic.gateway;

import com.carddemo.programs.cotrtlic.copybook.DclTransactionType;

/** The embedded SQL issued by COTRTLIC against CARDDEMO.TRANSACTION_TYPE (DB2). */
public interface TransactionTypeDb2Gateway {

    /** 9998-PRIMING-QUERY: SELECT 1 FROM SYSIBM.SYSDUMMY1 FETCH FIRST 1 ROW ONLY. */
    SqlResult<Integer> primingQuery();

    /** 9100-CHECK-FILTERS: SELECT COUNT(1) ... WHERE type/description filters. */
    SqlResult<Integer> countByFilters(TypeFilter filter);

    /** 9400: OPEN C-TR-TYPE-FORWARD (TR_TYPE &gt;= :WS-START-KEY ... ORDER BY TR_TYPE). */
    SqlResult<Void> openForwardCursor(TypeFilter filter);

    /** FETCH C-TR-TYPE-FORWARD INTO :DCL-TR-TYPE, :DCL-TR-DESCRIPTION. */
    SqlResult<DclTransactionType> fetchForward();

    /** 9450: CLOSE C-TR-TYPE-FORWARD. */
    SqlResult<Void> closeForwardCursor();

    /** 9500: OPEN C-TR-TYPE-BACKWARD (TR_TYPE &lt; :WS-START-KEY ... ORDER BY TR_TYPE DESC). */
    SqlResult<Void> openBackwardCursor(TypeFilter filter);

    /** FETCH C-TR-TYPE-BACKWARD INTO :DCL-TR-TYPE, :DCL-TR-DESCRIPTION. */
    SqlResult<DclTransactionType> fetchBackward();

    /** 9550: CLOSE C-TR-TYPE-BACKWARD. */
    SqlResult<Void> closeBackwardCursor();

    /** 9200: UPDATE ... SET TR_DESCRIPTION = :DCL-TR-DESCRIPTION WHERE TR_TYPE = :DCL-TR-TYPE. */
    SqlResult<Void> updateDescription(DclTransactionType hostVariables);

    /** 9300: DELETE FROM CARDDEMO.TRANSACTION_TYPE WHERE TR_TYPE = :DCL-TR-TYPE. */
    SqlResult<Void> deleteByType(String trType);
}
