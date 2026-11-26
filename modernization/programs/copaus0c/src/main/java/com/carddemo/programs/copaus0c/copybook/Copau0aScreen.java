package com.carddemo.programs.copaus0c.copybook;

import java.util.List;

/** BMS response DTO: the COPAU0AO image sent by EXEC CICS SEND MAP('COPAU0A') MAPSET('COPAU00'). */
public record Copau0aScreen(
        String trnname, String title01, String curdate, String pgmname, String title02, String curtime,
        String acctid, String cname, String custid, String addr001, String accstat, String addr002,
        String phone1, String apprcnt, String declcnt, String credlim, String cashlim, String appramt,
        String credbal, String cashbal, String declamt, List<Row> rows, String errmsg,
        boolean erase, String cursorField) {

    public Copau0aScreen {
        rows = List.copyOf(rows);
    }

    /** One list row: SEL000n (data + attribute), TRNID0n, PDATE0n, PTIME0n, PTYPE0n, PAPRV0n, PSTAT0n, PAMT00n. */
    public record Row(String sel, char selAttr, String trnid, String pdate, String ptime, String ptype,
                      String paprv, String pstat, String pamt) {
    }

    public Row row(int n) {
        return rows.get(n - 1);
    }
}
