package com.carddemo.programs.coactupc.copybook;

import com.carddemo.programs.coactupc.support.Cobol;

/** CSUTLDWY - working storage for the CCYYMMDD date edit routines (CSUTLDPY). */
public final class DateEditWorkArea {

    /**
     * WS-EDIT-DATE-CCYYMMDD PIC X(8): WS-EDIT-DATE-CC X(2) / WS-EDIT-DATE-YY X(2) /
     * WS-EDIT-DATE-MM X(2) / WS-EDIT-DATE-DD X(2), each redefined as PIC 9(2); redefined as a
     * whole by WS-EDIT-DATE-CCYYMMDD-N PIC 9(8).
     */
    public String editDateCcyymmdd = Cobol.spaces(8);
    /** WS-EDIT-DATE-BINARY PIC S9(9) BINARY. */
    public int editDateBinary;
    /** WS-CURRENT-DATE-YYYYMMDD PIC X(8) (redefined as 9(8)). */
    public String currentDateYyyymmdd = Cobol.spaces(8);
    /** WS-CURRENT-DATE-BINARY PIC S9(9) BINARY. */
    public int currentDateBinary;
    /** WS-EDIT-YEAR-FLG PIC X(01). */
    public char yearFlg = ' ';
    /** WS-EDIT-MONTH PIC X(01). */
    public char monthFlg = ' ';
    /** WS-EDIT-DAY PIC X(01). */
    public char dayFlg = ' ';
    /** WS-DATE-FORMAT PIC X(08) VALUE 'YYYYMMDD'. */
    public String dateFormat = "YYYYMMDD";
    /** WS-DATE-VALIDATION-RESULT: WS-SEVERITY PIC X(04). */
    public String severity = Cobol.spaces(4);
    /** WS-MSG-NO PIC X(04). */
    public String msgNo = Cobol.spaces(4);
    /** WS-RESULT PIC X(15). */
    public String result = Cobol.spaces(15);
    /** WS-DATE PIC X(10). */
    public String date = Cobol.spaces(10);
    /** WS-DATE-FMT PIC X(10). */
    public String dateFmt = Cobol.spaces(10);

    public static final char FLG_ISVALID = Cobol.LOW_VALUE;
    public static final char FLG_NOT_OK = '0';
    public static final char FLG_BLANK = 'B';

    /** INITIALIZE (as part of the enclosing WS-MISC-STORAGE group). */
    public void initialize() {
        editDateCcyymmdd = Cobol.spaces(8);
        editDateBinary = 0;
        currentDateYyyymmdd = Cobol.spaces(8);
        currentDateBinary = 0;
        yearFlg = ' ';
        monthFlg = ' ';
        dayFlg = ' ';
        dateFormat = Cobol.spaces(8);
        initializeValidationResult();
    }

    public void initializeValidationResult() {
        severity = Cobol.spaces(4);
        msgNo = Cobol.spaces(4);
        result = Cobol.spaces(15);
        date = Cobol.spaces(10);
        dateFmt = Cobol.spaces(10);
    }

    public String cc() {
        return editDateCcyymmdd.substring(0, 2);
    }

    public String yy() {
        return editDateCcyymmdd.substring(2, 4);
    }

    public String ccyy() {
        return editDateCcyymmdd.substring(0, 4);
    }

    public String mm() {
        return editDateCcyymmdd.substring(4, 6);
    }

    public String dd() {
        return editDateCcyymmdd.substring(6, 8);
    }

    public void setMm(String value) {
        editDateCcyymmdd = Cobol.replace(editDateCcyymmdd, 4, Cobol.pad(value, 2));
    }

    public void setDd(String value) {
        editDateCcyymmdd = Cobol.replace(editDateCcyymmdd, 6, Cobol.pad(value, 2));
    }

    public int ccN() {
        return (int) Cobol.unsignedValue(cc());
    }

    public int yyN() {
        return (int) Cobol.unsignedValue(yy());
    }

    public int ccyyN() {
        return (int) Cobol.unsignedValue(ccyy());
    }

    public int mmN() {
        return (int) Cobol.unsignedValue(mm());
    }

    public int ddN() {
        return (int) Cobol.unsignedValue(dd());
    }

    public long ccyymmddN() {
        return Cobol.unsignedValue(editDateCcyymmdd);
    }

    /** 88 THIS-CENTURY VALUE 20. */
    public boolean isThisCentury() {
        return ccN() == 20;
    }

    /** 88 LAST-CENTURY VALUE 19. */
    public boolean isLastCentury() {
        return ccN() == 19;
    }

    /** 88 WS-VALID-MONTH VALUES 1 THROUGH 12. */
    public boolean isValidMonth() {
        int m = mmN();
        return m >= 1 && m <= 12;
    }

    /** 88 WS-31-DAY-MONTH VALUES 1, 3, 5, 7, 8, 10, 12. */
    public boolean is31DayMonth() {
        int m = mmN();
        return m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12;
    }

    /** 88 WS-FEBRUARY VALUE 2. */
    public boolean isFebruary() {
        return mmN() == 2;
    }

    /** 88 WS-VALID-DAY VALUES 1 THROUGH 31. */
    public boolean isValidDay() {
        int d = ddN();
        return d >= 1 && d <= 31;
    }

    /** 88 WS-DAY-31. */
    public boolean isDay31() {
        return ddN() == 31;
    }

    /** 88 WS-DAY-30. */
    public boolean isDay30() {
        return ddN() == 30;
    }

    /** 88 WS-DAY-29. */
    public boolean isDay29() {
        return ddN() == 29;
    }

    /** 88 WS-VALID-FEB-DAY VALUES 1 THROUGH 28. */
    public boolean isValidFebDay() {
        int d = ddN();
        return d >= 1 && d <= 28;
    }

    /** WS-EDIT-DATE-FLGS PIC X(3) - 88 WS-EDIT-DATE-IS-VALID VALUE LOW-VALUES. */
    public boolean isDateValid() {
        return yearFlg == FLG_ISVALID && monthFlg == FLG_ISVALID && dayFlg == FLG_ISVALID;
    }

    /** SET WS-EDIT-DATE-IS-VALID TO TRUE. */
    public void setDateValid() {
        yearFlg = FLG_ISVALID;
        monthFlg = FLG_ISVALID;
        dayFlg = FLG_ISVALID;
    }

    /** 88 WS-EDIT-DATE-IS-INVALID VALUE '000'. */
    public boolean isDateInvalid() {
        return yearFlg == FLG_NOT_OK && monthFlg == FLG_NOT_OK && dayFlg == FLG_NOT_OK;
    }

    /** SET WS-EDIT-DATE-IS-INVALID TO TRUE. */
    public void setDateInvalid() {
        yearFlg = FLG_NOT_OK;
        monthFlg = FLG_NOT_OK;
        dayFlg = FLG_NOT_OK;
    }

    /** WS-EDIT-DATE-FLGS as the 3-byte group (year, month, day). */
    public char[] flags() {
        return new char[] {yearFlg, monthFlg, dayFlg};
    }
}
