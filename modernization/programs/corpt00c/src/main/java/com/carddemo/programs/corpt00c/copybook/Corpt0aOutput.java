package com.carddemo.programs.corpt00c.copybook;

/**
 * Response DTO for map CORPT0A (CORPT0AO data fields), plus ERRMSGC and the field that received the cursor
 * (the first field whose xxxL was -1, or null).
 */
public record Corpt0aOutput(
        String trnname,   // TRNNAMEO X(4)
        String title01,   // TITLE01O X(40)
        String curdate,   // CURDATEO X(8)
        String pgmname,   // PGMNAMEO X(8)
        String title02,   // TITLE02O X(40)
        String curtime,   // CURTIMEO X(8)
        String monthly,   // MONTHLYO X(1)
        String yearly,    // YEARLYO X(1)
        String custom,    // CUSTOMO X(1)
        String sdtmm,     // SDTMMO X(2)
        String sdtdd,     // SDTDDO X(2)
        String sdtyyyy,   // SDTYYYYO X(4)
        String edtmm,     // EDTMMO X(2)
        String edtdd,     // EDTDDO X(2)
        String edtyyyy,   // EDTYYYYO X(4)
        String confirm,   // CONFIRMO X(1)
        String errmsg,    // ERRMSGO X(78)
        char errmsgColor, // ERRMSGC
        String cursorField) {
}
