package com.carddemo.programs.copaus1c;

import java.util.List;
import java.util.Optional;

/** WS-DECLINE-REASON-TABLE / WS-DECLINE-REASON-TAB (OCCURS 10, ASCENDING KEY DECL-CODE). */
public final class DeclineReasonTable {

    /** One table entry: DECL-CODE PIC X(4), DECL-DESC PIC X(16). */
    public record Entry(String declCode, String declDesc) {
    }

    public static final List<Entry> ENTRIES = List.of(
            new Entry("0000", "APPROVED        "),
            new Entry("3100", "INVALID CARD    "),
            new Entry("4100", "INSUFFICNT FUND "),
            new Entry("4200", "CARD NOT ACTIVE "),
            new Entry("4300", "ACCOUNT CLOSED  "),
            new Entry("4400", "EXCED DAILY LMT "),
            new Entry("5100", "CARD FRAUD      "),
            new Entry("5200", "MERCHANT FRAUD  "),
            new Entry("5300", "LOST CARD       "),
            new Entry("9000", "UNKNOWN         "));

    private DeclineReasonTable() {
    }

    /** SEARCH ALL ... WHEN DECL-CODE(idx) = code: binary search on the ascending key. */
    public static Optional<Entry> searchAll(String code) {
        int lo = 0;
        int hi = ENTRIES.size() - 1;
        while (lo <= hi) {
            int mid = (lo + hi) >>> 1;
            int cmp = ENTRIES.get(mid).declCode().compareTo(code);
            if (cmp == 0) {
                return Optional.of(ENTRIES.get(mid));
            }
            if (cmp < 0) {
                lo = mid + 1;
            } else {
                hi = mid - 1;
            }
        }
        return Optional.empty();
    }
}
