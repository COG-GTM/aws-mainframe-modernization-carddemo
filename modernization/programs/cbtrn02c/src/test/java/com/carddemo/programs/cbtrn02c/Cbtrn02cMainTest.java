package com.carddemo.programs.cbtrn02c;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class Cbtrn02cMainTest {

    @Test
    void missingDdReturnsTwelve() {
        assertEquals(12, Cbtrn02cMain.execute(new String[] {"DALYTRAN=x"}));
    }

    @Test
    void missingInputDatasetAbendsWith999() {
        assertEquals(999, Cbtrn02cMain.execute(new String[] {"DALYTRAN=/nonexistent/d", "TRANFILE=/tmp/t",
                "XREFFILE=/tmp/x", "DALYREJS=/tmp/r", "ACCTFILE=/tmp/a", "TCATBALF=/tmp/c"}));
    }
}
