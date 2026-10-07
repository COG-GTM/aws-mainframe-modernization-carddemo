package com.carddemo.web.transaction;

import com.carddemo.web.NavigationContext;

/** Transaction ids and programs of the four screens and the PF3 targets. */
final class TransactionNavigation {

    static final String LIST_TRAN = "CT00";
    static final String LIST_PROGRAM = "COTRN00C";
    static final String VIEW_TRAN = "CT01";
    static final String VIEW_PROGRAM = "COTRN01C";
    static final String ADD_TRAN = "CT02";
    static final String ADD_PROGRAM = "COTRN02C";
    static final String BILL_PAY_TRAN = "CB00";
    static final String BILL_PAY_PROGRAM = "COBIL00C";
    static final String MENU_TRAN = "CM00";
    static final String MENU_PROGRAM = "COMEN01C";

    private TransactionNavigation() {
    }

    /** PF3: {@code CDEMO-FROM-PROGRAM} when it is the list, else the main menu. */
    static NavigationContext exit(String tranId, String program, String fromProgram) {
        if (LIST_PROGRAM.equals(fromProgram) && !LIST_PROGRAM.equals(program)) {
            return NavigationContext.transfer(tranId, program, LIST_TRAN, LIST_PROGRAM);
        }
        return NavigationContext.transfer(tranId, program, MENU_TRAN, MENU_PROGRAM);
    }
}
