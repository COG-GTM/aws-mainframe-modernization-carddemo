package com.carddemo.programs.copaus0c.copybook;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * BMS request DTO: the operator-modifiable fields of COPAU0AI (ACCTIDI, SEL0001I-SEL0005I).
 * A {@code null} value means "field not modified" (received as nulls).
 * {@code fsetEcho} holds the protected FSET fields the terminal sends back unchanged (by BMS field name).
 */
public record Copau0aInput(String acctId, String sel1, String sel2, String sel3, String sel4, String sel5,
                           Map<String, String> fsetEcho) {

    public Copau0aInput {
        fsetEcho = Map.copyOf(fsetEcho);
    }

    public Copau0aInput(String acctId, String sel1, String sel2, String sel3, String sel4, String sel5) {
        this(acctId, sel1, sel2, sel3, sel4, sel5, Map.of());
    }

    /** Returns a copy whose protected FSET fields (ATTRB=(ASKIP,FSET) in COPAU00.bms) echo {@code screen}. */
    public Copau0aInput withFsetFrom(Copau0aScreen screen) {
        Map<String, String> echo = new LinkedHashMap<>();
        echo.put("TRNNAME", screen.trnname());
        echo.put("TITLE01", screen.title01());
        echo.put("CURDATE", screen.curdate());
        echo.put("PGMNAME", screen.pgmname());
        echo.put("TITLE02", screen.title02());
        echo.put("CURTIME", screen.curtime());
        echo.put("CREDLIM", screen.credlim());
        echo.put("CASHLIM", screen.cashlim());
        echo.put("APPRAMT", screen.appramt());
        echo.put("CREDBAL", screen.credbal());
        echo.put("CASHBAL", screen.cashbal());
        echo.put("DECLAMT", screen.declamt());
        List<Copau0aScreen.Row> rows = screen.rows();
        for (int n = 1; n <= rows.size(); n++) {
            Copau0aScreen.Row r = rows.get(n - 1);
            echo.put("TRNID0" + n, r.trnid());
            echo.put("PDATE0" + n, r.pdate());
            echo.put("PTIME0" + n, r.ptime());
            echo.put("PTYPE0" + n, r.ptype());
            echo.put("PAPRV0" + n, r.paprv());
            echo.put("PSTAT0" + n, r.pstat());
            echo.put("PAMT00" + n, r.pamt());
        }
        echo.put("ERRMSG", screen.errmsg());
        echo.values().removeIf(v -> v.chars().allMatch(c -> c == 0));
        return new Copau0aInput(acctId, sel1, sel2, sel3, sel4, sel5, echo);
    }

    public static Copau0aInput of(String acctId) {
        return new Copau0aInput(acctId, null, null, null, null, null);
    }

    public static Copau0aInput none() {
        return new Copau0aInput(null, null, null, null, null, null);
    }

    /** Returns a copy with SEL000n set (1-based). */
    public Copau0aInput withSelection(int n, String value) {
        return new Copau0aInput(acctId, n == 1 ? value : sel1, n == 2 ? value : sel2, n == 3 ? value : sel3,
                n == 4 ? value : sel4, n == 5 ? value : sel5, fsetEcho);
    }

    public String selection(int n) {
        return switch (n) {
            case 1 -> sel1;
            case 2 -> sel2;
            case 3 -> sel3;
            case 4 -> sel4;
            case 5 -> sel5;
            default -> throw new IllegalArgumentException("SEL000" + n);
        };
    }
}
