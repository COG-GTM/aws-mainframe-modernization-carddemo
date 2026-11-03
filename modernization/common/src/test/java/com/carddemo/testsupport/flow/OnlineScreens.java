package com.carddemo.testsupport.flow;

import java.util.EnumSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

/**
 * The 17 in-scope CICS programs (scope.md §2.1), the service that replaces each and the screen keys
 * that need a flow test. Keys come from each program's {@code EVALUATE EIBAID} (DFHENTER, DFHPF5,
 * DFHPF7, DFHPF8, or CCARD-AID-ENTER/PFK05/PFK07/PFK08); PF3/PF4/PF12 are left out (client navigation).
 * COTRN01C's PF5 only XCTLs to COTRN00C, so it is navigation too; COTRN02C's PF5 reads the last
 * TRANSACT record (COPY-LAST-TRAN-DATA), which the API serves as a plain list read.
 */
public final class OnlineScreens {

    public record Screen(String program, String transactionId, String service, Set<ScreenKey> keys) {
    }

    public static final List<Screen> ALL = List.of(
            screen("COSGN00C", "CC00", "auth-service", ScreenKey.ENTER),
            screen("COMEN01C", "CM00", "auth-service", ScreenKey.ENTER),
            screen("COADM01C", "CA00", "auth-service", ScreenKey.ENTER),
            screen("COUSR00C", "CU00", "auth-service", ScreenKey.ENTER, ScreenKey.PF7, ScreenKey.PF8),
            screen("COUSR01C", "CU01", "auth-service", ScreenKey.ENTER),
            screen("COUSR02C", "CU02", "auth-service", ScreenKey.ENTER, ScreenKey.PF5),
            screen("COUSR03C", "CU03", "auth-service", ScreenKey.ENTER, ScreenKey.PF5),
            screen("COACTVWC", "CAVW", "account-service", ScreenKey.ENTER),
            screen("COACTUPC", "CAUP", "account-service", ScreenKey.ENTER, ScreenKey.PF5),
            screen("COCRDLIC", "CCLI", "card-service", ScreenKey.ENTER, ScreenKey.PF7, ScreenKey.PF8),
            screen("COCRDSLC", "CCDL", "card-service", ScreenKey.ENTER),
            screen("COCRDUPC", "CCUP", "card-service", ScreenKey.ENTER, ScreenKey.PF5),
            screen("COTRN00C", "CT00", "transaction-service", ScreenKey.ENTER, ScreenKey.PF7, ScreenKey.PF8),
            screen("COTRN01C", "CT01", "transaction-service", ScreenKey.ENTER),
            screen("COTRN02C", "CT02", "transaction-service", ScreenKey.ENTER, ScreenKey.PF5),
            screen("COBIL00C", "CB00", "transaction-service", ScreenKey.ENTER),
            screen("CORPT00C", "CR00", "batch-service", ScreenKey.ENTER));

    private OnlineScreens() {
    }

    public static Optional<Screen> find(String program) {
        return ALL.stream().filter(s -> s.program().equals(program)).findFirst();
    }

    public static List<Screen> ofService(String service) {
        return ALL.stream().filter(s -> s.service().equals(service)).toList();
    }

    private static Screen screen(String program, String transactionId, String service, ScreenKey first,
                                 ScreenKey... rest) {
        return new Screen(program, transactionId, service, Set.copyOf(EnumSet.of(first, rest)));
    }
}
