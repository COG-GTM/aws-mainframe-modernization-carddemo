package com.carddemo.programs.cocrdlic.copybook;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * COCRDLI symbolic map (app/cpy-bms/COCRDLI.CPY), map CCRDLIA: CCRDLIAI / CCRDLIAO share storage, so one
 * object models both the received input and the output sent by SEND MAP.
 */
public final class CardListMap {
    public static final int ROWS = 7;

    public final BmsField trnname = new BmsField("TRNNAME", 4);
    public final BmsField title01 = new BmsField("TITLE01", 40);
    public final BmsField curdate = new BmsField("CURDATE", 8);
    public final BmsField pgmname = new BmsField("PGMNAME", 8);
    public final BmsField title02 = new BmsField("TITLE02", 40);
    public final BmsField curtime = new BmsField("CURTIME", 8);
    public final BmsField pageno = new BmsField("PAGENO", 3);
    public final BmsField acctsid = new BmsField("ACCTSID", 11);
    public final BmsField cardsid = new BmsField("CARDSID", 16);
    private final BmsField[] crdsel = new BmsField[ROWS];
    /** CRDSTP2..CRDSTP7 exist in the map (rows 2-7 only) but are never referenced by COCRDLIC. */
    private final BmsField[] crdstp = new BmsField[ROWS];
    private final BmsField[] acctno = new BmsField[ROWS];
    private final BmsField[] crdnum = new BmsField[ROWS];
    private final BmsField[] crdsts = new BmsField[ROWS];
    public final BmsField infomsg = new BmsField("INFOMSG", 45);
    public final BmsField errmsg = new BmsField("ERRMSG", 78);

    private final List<BmsField> mapOrder;

    public CardListMap() {
        List<BmsField> order = new ArrayList<>(List.of(trnname, title01, curdate, pgmname, title02, curtime,
                pageno, acctsid, cardsid));
        for (int i = 0; i < ROWS; i++) {
            int n = i + 1;
            crdsel[i] = new BmsField("CRDSEL" + n, 1);
            acctno[i] = new BmsField("ACCTNO" + n, 11);
            crdnum[i] = new BmsField("CRDNUM" + n, 16);
            crdsts[i] = new BmsField("CRDSTS" + n, 1);
            order.add(crdsel[i]);
            if (n > 1) {
                crdstp[i] = new BmsField("CRDSTP" + n, 1);
                order.add(crdstp[i]);
            }
            order.add(acctno[i]);
            order.add(crdnum[i]);
            order.add(crdsts[i]);
        }
        order.add(infomsg);
        order.add(errmsg);
        mapOrder = Collections.unmodifiableList(order);
    }

    /** MOVE LOW-VALUES TO CCRDLIAO. */
    public void clear() {
        mapOrder.forEach(BmsField::clear);
    }

    /** Row accessors are 1-based like the COBOL suffixes. */
    public BmsField crdsel(int row) { return crdsel[row - 1]; }
    public BmsField acctno(int row) { return acctno[row - 1]; }
    public BmsField crdnum(int row) { return crdnum[row - 1]; }
    public BmsField crdsts(int row) { return crdsts[row - 1]; }

    public List<BmsField> fieldsInMapOrder() {
        return mapOrder;
    }

    /** SEND MAP ... CURSOR: the cursor goes to the first field (in map order) whose length was set to -1. */
    public String cursorField() {
        for (BmsField f : mapOrder) {
            if (f.hasCursor()) {
                return f.name();
            }
        }
        return null;
    }
}
