package com.carddemo.programs.corpt00c;

import java.util.ArrayList;
import java.util.List;

/**
 * 01 JOB-DATA of CORPT00C: the in-line JCL deck (JOB-DATA-1) submitted line by line to the JOBS TD queue,
 * addressed through JOB-DATA-2 / JOB-LINES OCCURS 1000 TIMES PIC X(80). It runs PROC TRANREPT
 * (app/proc/TRANREPT.prc) with SORT SYMNAMES overrides for the report date range.
 */
public final class TranReportJob {

    public static final int LINE_LENGTH = 80;
    public static final int MAX_LINES = 1000;

    private String parmStartDate1 = CobolText.spaces(10);
    private String parmEndDate1 = CobolText.spaces(10);
    private String parmStartDate2 = CobolText.spaces(10);
    private String parmEndDate2 = CobolText.spaces(10);

    /** MOVE date TO PARM-START-DATE-1 PARM-START-DATE-2. */
    public void setStartDate(String date) {
        parmStartDate1 = CobolText.move(date, 10);
        parmStartDate2 = parmStartDate1;
    }

    /** MOVE date TO PARM-END-DATE-1 PARM-END-DATE-2. */
    public void setEndDate(String date) {
        parmEndDate1 = CobolText.move(date, 10);
        parmEndDate2 = parmEndDate1;
    }

    /** The JOB-DATA-1 lines, each exactly 80 characters. */
    public List<String> lines() {
        List<String> l = new ArrayList<>();
        l.add(line("//TRNRPT00 JOB 'TRAN REPORT',CLASS=A,MSGCLASS=0,"));
        l.add(line("// NOTIFY=&SYSUID"));
        l.add(line("//*"));
        l.add(line("//JOBLIB JCLLIB ORDER=('AWS.M2.CARDDEMO.PROC')"));
        l.add(line("//*"));
        l.add(line("//STEP10 EXEC PROC=TRANREPT"));
        l.add(line("//*"));
        l.add(line("//STEP05R.SYMNAMES DD *"));
        l.add(line("TRAN-CARD-NUM,263,16,ZD"));
        l.add(line("TRAN-PROC-DT,305,10,CH"));
        // FILLER-1: X(18) "PARM-START-DATE,C'" + PARM-START-DATE-1 X(10) + X(52) "'"
        l.add(CobolText.move("PARM-START-DATE,C'", 18) + parmStartDate1 + CobolText.move("'", 52));
        // FILLER-2: X(16) "PARM-END-DATE,C'" + PARM-END-DATE-1 X(10) + X(54) "'"
        l.add(CobolText.move("PARM-END-DATE,C'", 16) + parmEndDate1 + CobolText.move("'", 54));
        l.add(line("/*"));
        l.add(line("//STEP10R.DATEPARM DD *"));
        // FILLER-3: PARM-START-DATE-2 X(10) + ' ' + PARM-END-DATE-2 X(10) + X(59)
        l.add(parmStartDate2 + " " + parmEndDate2 + CobolText.spaces(59));
        l.add(line("/*"));
        l.add(line("/*EOF"));
        return l;
    }

    /**
     * JOB-LINES(idx), 1-based. Entries past the end of JOB-DATA-1 lie outside the defined storage in COBOL;
     * they are returned as LOW-VALUES so a scan beyond the deck terminates.
     */
    public String jobLine(int idx) {
        List<String> l = lines();
        if (idx < 1 || idx > MAX_LINES) {
            throw new IndexOutOfBoundsException("JOB-LINES subscript " + idx);
        }
        return idx <= l.size() ? l.get(idx - 1) : CobolText.lowValues(LINE_LENGTH);
    }

    private static String line(String s) {
        return CobolText.move(s, LINE_LENGTH);
    }
}
