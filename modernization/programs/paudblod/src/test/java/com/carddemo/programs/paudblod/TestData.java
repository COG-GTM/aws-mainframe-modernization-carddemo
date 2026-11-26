package com.carddemo.programs.paudblod;

import com.carddemo.programs.paudblod.copybook.CobolFields;
import com.carddemo.programs.paudblod.copybook.ImsFuncs;
import com.carddemo.programs.paudblod.copybook.PautbPcb;
import com.carddemo.programs.paudblod.copybook.PendingAuthDetails;
import com.carddemo.programs.paudblod.copybook.PendingAuthSummary;
import com.carddemo.programs.paudblod.ims.DliGateway;
import com.carddemo.programs.paudblod.ims.Ssa;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.function.BiPredicate;

final class TestData {

    private TestData() {
    }

    static PendingAuthSummary summary(long acctId) {
        return new PendingAuthSummary(BigDecimal.valueOf(acctId), 100000000L + acctId, "A",
                List.of("AC", "  ", "  ", "  ", "  "),
                new BigDecimal("5000.00"), new BigDecimal("1000.00"), new BigDecimal("125.50"),
                new BigDecimal("0.00"), 3, 1, new BigDecimal("300.25"), new BigDecimal("75.00"), "");
    }

    static PendingAuthDetails details(long time, String txnId) {
        return new PendingAuthDetails(new BigDecimal("76699"), BigDecimal.valueOf(time), "231027", "101500",
                "4111111111111111", "PURC", "1225", "AUTHRQ", "POS   ", "A1B2C3", "00", "0000",
                3000L, new BigDecimal("42.17"), new BigDecimal("42.17"), "5411", "USA", 5,
                "MERCH0000000001", "TEST MERCHANT", "SPRINGFIELD", "IL", "62701", txnId,
                "P", " ", "", "");
    }

    static byte[] childRecord(BigDecimal rootKey, byte[] child) {
        byte[] rec = new byte[Paudblod.INFIL2_REC_LENGTH];
        CobolFields.encodePacked(rootKey, 11, 0, rec, 0);
        System.arraycopy(child, 0, rec, Paudblod.ROOT_SEG_KEY_LENGTH, child.length);
        return rec;
    }

    /** One recorded CBLTDLI call. */
    record Call(ImsFuncs function, Ssa ssa, String statusBefore) {
    }

    /** Wraps a gateway, records every call and can force the status of matching calls. */
    static final class ScriptedGateway implements DliGateway {
        final List<Call> calls = new ArrayList<>();
        private final DliGateway delegate;
        private BiPredicate<ImsFuncs, Ssa> match = (f, s) -> false;
        private String forcedStatus;

        ScriptedGateway(DliGateway delegate) {
            this.delegate = delegate;
        }

        ScriptedGateway force(BiPredicate<ImsFuncs, Ssa> when, String status) {
            this.match = when;
            this.forcedStatus = status;
            return this;
        }

        @Override
        public void cbltdli(ImsFuncs function, PautbPcb pcb, byte[] ioArea, Ssa ssa) {
            calls.add(new Call(function, ssa, pcb.getPautPcbStatus()));
            if (match.test(function, ssa)) {
                pcb.setPautPcbStatus(forcedStatus);
                return;
            }
            delegate.cbltdli(function, pcb, ioArea, ssa);
        }
    }
}
