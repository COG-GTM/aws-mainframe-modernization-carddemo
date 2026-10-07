package com.carddemo.web.user;

import com.carddemo.common.online.OnlineProgram;
import com.carddemo.web.NavigationContext;

/** Transaction ids and programs of the four user screens and their PF3/PF12 target (COADM01C). */
final class UserNavigation {

    static final String LIST_TRAN = "CU00";
    static final String LIST_PROGRAM = "COUSR00C";
    static final String ADD_TRAN = "CU01";
    static final String ADD_PROGRAM = "COUSR01C";
    static final String UPDATE_TRAN = "CU02";
    static final String UPDATE_PROGRAM = "COUSR02C";
    static final String DELETE_TRAN = "CU03";
    static final String DELETE_PROGRAM = "COUSR03C";
    static final String ADMIN_TRAN = "CA00";
    static final String ADMIN_PROGRAM = "COADM01C";

    private UserNavigation() {
    }

    /** COUSR00C R-5, COUSR01C R-4: PF3 always returns to the admin menu. */
    static NavigationContext adminMenu(String tranId, String program) {
        return NavigationContext.transfer(tranId, program, ADMIN_TRAN, ADMIN_PROGRAM);
    }

    /** COUSR02C R-5, COUSR03C R-5: {@code CDEMO-FROM-PROGRAM} when it names another installed program, else COADM01C. */
    static NavigationContext exit(String tranId, String program, String fromProgram) {
        String caller = fromProgram == null ? "" : fromProgram.strip();
        return OnlineProgram.find(caller)
                .filter(p -> !p.programId().equals(program))
                .map(p -> NavigationContext.transfer(tranId, program, p.tranId(), p.programId()))
                .orElseGet(() -> adminMenu(tranId, program));
    }
}
