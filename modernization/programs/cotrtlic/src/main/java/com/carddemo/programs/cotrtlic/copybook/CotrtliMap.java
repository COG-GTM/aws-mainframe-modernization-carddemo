package com.carddemo.programs.cotrtlic.copybook;

import java.util.ArrayList;
import java.util.List;

/**
 * Symbolic map COTRTLI.cpy (mapset COTRTLI, map CTRTLIA): CTRTLIAI / CTRTLIAO REDEFINES CTRTLIAI,
 * including the program's OCCURS 7 overlay (EACH-ROWI / EACH-ROWO) of TRTSELn / TRTTYPn / TRTYPDn.
 */
public final class CotrtliMap {

    public static final int ROWS = 7;

    /** One list row: TRTSEL(I) X(1), TRTTYP(I) X(2), TRTYPD(I) X(50). */
    public static final class Row {
        public final BmsField trtsel;
        public final BmsField trttyp;
        public final BmsField trtypd;

        Row() {
            this(new BmsField(1), new BmsField(2), new BmsField(50));
        }

        private Row(BmsField trtsel, BmsField trttyp, BmsField trtypd) {
            this.trtsel = trtsel;
            this.trttyp = trttyp;
            this.trtypd = trtypd;
        }

        Row copy() {
            return new Row(trtsel.copy(), trttyp.copy(), trtypd.copy());
        }
    }

    public final BmsField trnname = new BmsField(4);
    public final BmsField title01 = new BmsField(40);
    public final BmsField curdate = new BmsField(8);
    public final BmsField pgmname = new BmsField(8);
    public final BmsField title02 = new BmsField(40);
    public final BmsField curtime = new BmsField(8);
    public final BmsField pageno = new BmsField(3);
    /** Type code filter TRTYPE X(2). */
    public final BmsField trtype = new BmsField(2);
    /** Description filter TRDESC X(50). */
    public final BmsField trdesc = new BmsField(50);
    private final Row[] rows = new Row[ROWS];
    /** Column-heading underline fields. */
    public final BmsField trtsela = new BmsField(1);
    public final BmsField trttypa = new BmsField(2);
    public final BmsField trtdsca = new BmsField(50);
    public final BmsField infomsg = new BmsField(45);
    public final BmsField errmsg = new BmsField(78);
    public final BmsField butnf02 = new BmsField(7);
    public final BmsField butnf03 = new BmsField(7);
    public final BmsField butnf07 = new BmsField(10);
    public final BmsField butnf08 = new BmsField(10);
    public final BmsField butnf10 = new BmsField(8);

    public CotrtliMap() {
        for (int i = 0; i < ROWS; i++) {
            rows[i] = new Row();
        }
    }

    /** EACH-ROWI / EACH-ROWO (I), 1-based as in COBOL. */
    public Row row(int i) {
        return rows[i - 1];
    }

    public List<BmsField> allFields() {
        List<BmsField> all = new ArrayList<>(List.of(trnname, title01, curdate, pgmname, title02, curtime,
                pageno, trtype, trdesc));
        for (Row r : rows) {
            all.add(r.trtsel);
            all.add(r.trttyp);
            all.add(r.trtypd);
        }
        all.addAll(List.of(trtsela, trttypa, trtdsca, infomsg, errmsg, butnf02, butnf03, butnf07, butnf08,
                butnf10));
        return all;
    }

    /** MOVE LOW-VALUES TO CTRTLIAO. */
    public void clear() {
        allFields().forEach(BmsField::clear);
    }

    public CotrtliMap copy() {
        CotrtliMap m = new CotrtliMap();
        List<BmsField> src = allFields();
        List<BmsField> dst = m.allFields();
        for (int i = 0; i < src.size(); i++) {
            BmsField s = src.get(i);
            BmsField d = dst.get(i);
            d.length = s.length;
            d.attribute = s.attribute;
            d.color = s.color;
            d.ps = s.ps;
            d.highlight = s.highlight;
            d.validation = s.validation;
            d.setRaw(s.data());
        }
        return m;
    }

    /** EXEC CICS RECEIVE MAP INTO(CTRTLIAI): overwrite this map with the terminal data. */
    public void receiveFrom(CotrtliMap terminal) {
        CotrtliMap c = terminal.copy();
        List<BmsField> src = c.allFields();
        List<BmsField> dst = allFields();
        for (int i = 0; i < src.size(); i++) {
            BmsField s = src.get(i);
            BmsField d = dst.get(i);
            d.length = s.length;
            d.attribute = s.attribute;
            d.color = s.color;
            d.ps = s.ps;
            d.highlight = s.highlight;
            d.validation = s.validation;
            d.setRaw(s.data());
        }
    }
}
