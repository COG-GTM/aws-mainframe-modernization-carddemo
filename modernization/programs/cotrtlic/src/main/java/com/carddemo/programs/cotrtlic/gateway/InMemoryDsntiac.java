package com.carddemo.programs.cotrtlic.gateway;

import com.carddemo.programs.cotrtlic.copybook.Sqlca;

/**
 * In-memory DSNTIAC following the documented DSNTIAR return codes: 8 when LRECL is outside 72-240,
 * 12 when the message area is shorter than 240 bytes, otherwise 0 with a DSNT408I-style text.
 */
public final class InMemoryDsntiac implements Dsntiac {

    @Override
    public Result format(Sqlca sqlca, int messageLength, int lrecl) {
        if (lrecl < 72 || lrecl > 240) {
            return new Result(8, null);
        }
        if (messageLength < 240) {
            return new Result(12, null);
        }
        String line1 = "DSNT408I SQLCODE = " + sqlca.sqlcode + ", ERROR: " + sqlca.sqlerrmc;
        return new Result(0, line1);
    }
}
