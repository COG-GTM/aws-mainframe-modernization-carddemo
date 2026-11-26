package com.carddemo.programs.coactupc;

import com.carddemo.programs.coactupc.copybook.CactupaField;
import com.carddemo.programs.coactupc.support.Cobol;

/**
 * Elementary edit flags of WS-NON-KEY-FLAGS, each PIC X(1) with
 * 88 FLG-xxx-ISVALID (LOW-VALUES, or 'Y'/'N' for the yes/no fields), FLG-xxx-NOT-OK '0'
 * and FLG-xxx-BLANK 'B'; mapped to the CACTUPA field they highlight.
 */
public enum NonKeyFlag {
    ACCT_STATUS(CactupaField.ACSTTUS, true),
    CRED_LIMIT(CactupaField.ACRDLIM, false),
    CASH_CREDIT_LIMIT(CactupaField.ACSHLIM, false),
    CURR_BAL(CactupaField.ACURBAL, false),
    CURR_CYC_CREDIT(CactupaField.ACRCYCR, false),
    CURR_CYC_DEBIT(CactupaField.ACRCYDB, false),
    DT_OF_BIRTH_YEAR(CactupaField.DOBYEAR, false),
    DT_OF_BIRTH_MONTH(CactupaField.DOBMON, false),
    DT_OF_BIRTH_DAY(CactupaField.DOBDAY, false),
    FICO_SCORE(CactupaField.ACSTFCO, false),
    OPEN_YEAR(CactupaField.OPNYEAR, false),
    OPEN_MONTH(CactupaField.OPNMON, false),
    OPEN_DAY(CactupaField.OPNDAY, false),
    EXPIRY_YEAR(CactupaField.EXPYEAR, false),
    EXPIRY_MONTH(CactupaField.EXPMON, false),
    EXPIRY_DAY(CactupaField.EXPDAY, false),
    REISSUE_YEAR(CactupaField.RISYEAR, false),
    REISSUE_MONTH(CactupaField.RISMON, false),
    REISSUE_DAY(CactupaField.RISDAY, false),
    FIRST_NAME(CactupaField.ACSFNAM, false),
    MIDDLE_NAME(CactupaField.ACSMNAM, false),
    LAST_NAME(CactupaField.ACSLNAM, false),
    ADDRESS_LINE_1(CactupaField.ACSADL1, false),
    ADDRESS_LINE_2(CactupaField.ACSADL2, false),
    CITY(CactupaField.ACSCITY, false),
    STATE(CactupaField.ACSSTTE, false),
    ZIPCODE(CactupaField.ACSZIPC, false),
    COUNTRY(CactupaField.ACSCTRY, false),
    PHONE_NUM_1A(CactupaField.ACSPH1A, false),
    PHONE_NUM_1B(CactupaField.ACSPH1B, false),
    PHONE_NUM_1C(CactupaField.ACSPH1C, false),
    PHONE_NUM_2A(CactupaField.ACSPH2A, false),
    PHONE_NUM_2B(CactupaField.ACSPH2B, false),
    PHONE_NUM_2C(CactupaField.ACSPH2C, false),
    EFT_ACCOUNT_ID(CactupaField.ACSEFTC, false),
    PRI_CARDHOLDER(CactupaField.ACSPFLG, true);

    public static final char ISVALID = Cobol.LOW_VALUE;
    public static final char NOT_OK = '0';
    public static final char BLANK = 'B';

    public final CactupaField screenField;
    private final boolean yesNo;

    NonKeyFlag(CactupaField screenField, boolean yesNo) {
        this.screenField = screenField;
        this.yesNo = yesNo;
    }

    public boolean isValid(char value) {
        return yesNo ? value == 'Y' || value == 'N' : value == ISVALID;
    }

    public boolean isNotOk(char value) {
        return value == NOT_OK;
    }

    public boolean isBlank(char value) {
        return value == BLANK;
    }
}
