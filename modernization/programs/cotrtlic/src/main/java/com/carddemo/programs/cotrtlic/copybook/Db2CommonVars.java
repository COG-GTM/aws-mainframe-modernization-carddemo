package com.carddemo.programs.cotrtlic.copybook;

import com.carddemo.programs.cotrtlic.support.Cob;
import java.math.BigDecimal;

/** Copybook CSDB2RWY: DB2 working-storage (WS-DB2-COMMON-VARS, WS-DSNTIAC-*). */
public final class Db2CommonVars {
    public static final int DSNTIAC_LINES = 10;
    public static final int DSNTIAC_LINE_LEN = 72;

    /** WS-DISP-SQLCODE PIC ----9 (numeric edited). */
    public String dispSqlcode;
    /** WS-DUMMY-DB2-INT PIC S9(4) COMP-3. */
    public BigDecimal dummyDb2Int;
    /** WS-DB2-PROCESSING-FLAG PIC X(1): 88 WS-DB2-OK '0', 88 WS-DB2-ERROR '1'. */
    public char db2ProcessingFlag;
    /** WS-DB2-CURRENT-ACTION PIC X(72). */
    public String db2CurrentAction;
    /** WS-DSNTIAC-MESG-LEN PIC S9(4) COMP VALUE +720. */
    public int dsntiacMesgLen;
    /** WS-DSNTIAC-FMTD-TEXT: WS-DSNTIAC-FMTD-TEXT-LINE PIC X(72) OCCURS 10 TIMES (720 bytes). */
    public String dsntiacFmtdText;
    /** WS-DSNTIAC-LRECL PIC S9(4) COMP VALUE +72. */
    public int dsntiacLrecl;
    /** WS-DSNTIAC-ERR-MSG PIC X(10) VALUE 'DSNTIAC CD'. */
    public String dsntiacErrMsg;
    /** WS-DSNTIAC-ERR-CD-X PIC X(02) (REDEFINES WS-DSNTIAC-ERR-CD PIC 9(02)). */
    public String dsntiacErrCdX;

    public Db2CommonVars() {
        dispSqlcode = Cob.editFloatingMinus5(0);
        dummyDb2Int = BigDecimal.ZERO;
        db2ProcessingFlag = ' ';
        db2CurrentAction = Cob.spaces(72);
        dsntiacMesgLen = 720;
        dsntiacFmtdText = Cob.spaces(DSNTIAC_LINES * DSNTIAC_LINE_LEN);
        dsntiacLrecl = 72;
        dsntiacErrMsg = "DSNTIAC CD";
        dsntiacErrCdX = Cob.spaces(2);
    }

    /** INITIALIZE (as part of WS-MISC-STORAGE): alphanumerics SPACES, numerics ZERO. */
    public void initialize() {
        dispSqlcode = Cob.editFloatingMinus5(0);
        dummyDb2Int = BigDecimal.ZERO;
        db2ProcessingFlag = ' ';
        db2CurrentAction = Cob.spaces(72);
        dsntiacMesgLen = 0;
        dsntiacFmtdText = Cob.spaces(DSNTIAC_LINES * DSNTIAC_LINE_LEN);
        dsntiacLrecl = 0;
        dsntiacErrMsg = Cob.spaces(10);
        dsntiacErrCdX = Cob.spaces(2);
    }

    public boolean isDb2Ok() {
        return db2ProcessingFlag == '0';
    }

    public boolean isDb2Error() {
        return db2ProcessingFlag == '1';
    }

    public void setDb2Error() {
        db2ProcessingFlag = '1';
    }

    public void setDb2Ok() {
        db2ProcessingFlag = '0';
    }

    /** MOVE SQLCODE TO WS-DISP-SQLCODE. */
    public void setDispSqlcode(int sqlcode) {
        dispSqlcode = Cob.editFloatingMinus5(sqlcode);
    }

    /** COMPUTE WS-DSNTIAC-ERR-CD = RETURN-CODE (PIC 9(02): unsigned, truncated). */
    public void setDsntiacErrCd(int returnCode) {
        dsntiacErrCdX = Cob.zeroPad(returnCode, 2);
    }

    /** WS-DSNTIAC-ERROR group (12 bytes). */
    public String dsntiacError() {
        return Cob.move(dsntiacErrMsg, 10) + Cob.move(dsntiacErrCdX, 2);
    }
}
