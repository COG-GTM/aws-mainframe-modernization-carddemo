package com.carddemo.programs.copaus1c;

import com.carddemo.programs.copaus1c.copybook.AuthorizationKey;
import com.carddemo.programs.copaus1c.copybook.Cobol;

import java.util.Arrays;

/** CDEMO-CPVD-INFO - the pending-authorization extension COPAUS1C appends to CARDDEMO-COMMAREA. */
public final class CpvdInfo {

    private char pauSelFlg = ' ';                                         // CDEMO-CPVD-PAU-SEL-FLG     X(01)
    private AuthorizationKey pauSelected = AuthorizationKey.SPACES;       // CDEMO-CPVD-PAU-SELECTED    X(08)
    private final AuthorizationKey[] paukeyPrevPg = filled();             // CDEMO-CPVD-PAUKEY-PREV-PG  X(08) OCCURS 20
    private AuthorizationKey paukeyLast = AuthorizationKey.SPACES;        // CDEMO-CPVD-PAUKEY-LAST     X(08)
    private int pageNum;                                                  // CDEMO-CPVD-PAGE-NUM        S9(04) COMP
    private char nextPageFlg = 'N';                                       // CDEMO-CPVD-NEXT-PAGE-FLG   X(01) VALUE 'N'
    private final AuthorizationKey[] authKeys = filled(5);                // CDEMO-CPVD-AUTH-KEYS       X(08) OCCURS 5
    private String fraudData = Cobol.spaces(100);                         // CDEMO-CPVD-FRAUD-DATA      X(100)

    private static AuthorizationKey[] filled() {
        return filled(20);
    }

    private static AuthorizationKey[] filled(int n) {
        AuthorizationKey[] a = new AuthorizationKey[n];
        Arrays.fill(a, AuthorizationKey.SPACES);
        return a;
    }

    /** INITIALIZE: alphanumerics to SPACES (VALUE clauses are not applied), numerics to ZERO. */
    public void initialize() {
        copyFrom(new CpvdInfo());
        nextPageFlg = ' ';
    }

    public void copyFrom(CpvdInfo o) {
        pauSelFlg = o.pauSelFlg;
        pauSelected = o.pauSelected;
        System.arraycopy(o.paukeyPrevPg, 0, paukeyPrevPg, 0, 20);
        paukeyLast = o.paukeyLast;
        pageNum = o.pageNum;
        nextPageFlg = o.nextPageFlg;
        System.arraycopy(o.authKeys, 0, authKeys, 0, 5);
        fraudData = o.fraudData;
    }

    public boolean isNextPageYes() { return nextPageFlg == 'Y'; }
    public boolean isNextPageNo() { return nextPageFlg == 'N'; }

    public char getPauSelFlg() { return pauSelFlg; }
    public void setPauSelFlg(char v) { pauSelFlg = v; }
    public AuthorizationKey getPauSelected() { return pauSelected; }
    public void setPauSelected(AuthorizationKey v) { pauSelected = v; }
    public AuthorizationKey getPaukeyPrevPg(int index1) { return paukeyPrevPg[index1 - 1]; }
    public void setPaukeyPrevPg(int index1, AuthorizationKey v) { paukeyPrevPg[index1 - 1] = v; }
    public AuthorizationKey getPaukeyLast() { return paukeyLast; }
    public void setPaukeyLast(AuthorizationKey v) { paukeyLast = v; }
    public int getPageNum() { return pageNum; }
    public void setPageNum(int v) { pageNum = v % 10000; }
    public char getNextPageFlg() { return nextPageFlg; }
    public void setNextPageFlg(char v) { nextPageFlg = v; }
    public AuthorizationKey getAuthKey(int index1) { return authKeys[index1 - 1]; }
    public void setAuthKey(int index1, AuthorizationKey v) { authKeys[index1 - 1] = v; }
    public String getFraudData() { return fraudData; }
    public void setFraudData(String v) { fraudData = Cobol.x(v, 100); }
}
