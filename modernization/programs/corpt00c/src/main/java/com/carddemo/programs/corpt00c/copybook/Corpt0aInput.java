package com.carddemo.programs.corpt00c.copybook;

/**
 * Request DTO for map CORPT0A (the unprotected CORPT0AI fields). A null value means the field was not
 * transmitted (received as LOW-VALUES).
 */
public record Corpt0aInput(
        String monthly,   // MONTHLYI X(1)
        String yearly,    // YEARLYI X(1)
        String custom,    // CUSTOMI X(1)
        String sdtmm,     // SDTMMI X(2)
        String sdtdd,     // SDTDDI X(2)
        String sdtyyyy,   // SDTYYYYI X(4)
        String edtmm,     // EDTMMI X(2)
        String edtdd,     // EDTDDI X(2)
        String edtyyyy,   // EDTYYYYI X(4)
        String confirm) { // CONFIRMI X(1)

    public static Corpt0aInput empty() {
        return new Corpt0aInput(null, null, null, null, null, null, null, null, null, null);
    }

    public static Corpt0aInput monthly(String confirm) {
        return new Corpt0aInput("X", null, null, null, null, null, null, null, null, confirm);
    }

    public static Corpt0aInput yearly(String confirm) {
        return new Corpt0aInput(null, "X", null, null, null, null, null, null, null, confirm);
    }

    public static Corpt0aInput custom(String sdtmm, String sdtdd, String sdtyyyy, String edtmm, String edtdd,
                                      String edtyyyy, String confirm) {
        return new Corpt0aInput(null, null, "X", sdtmm, sdtdd, sdtyyyy, edtmm, edtdd, edtyyyy, confirm);
    }
}
