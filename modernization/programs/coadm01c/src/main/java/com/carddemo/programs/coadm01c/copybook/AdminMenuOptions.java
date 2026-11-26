package com.carddemo.programs.coadm01c.copybook;

import java.util.List;

/** COADM02Y - 01 CARDDEMO-ADMIN-MENU-OPTIONS. */
public final class AdminMenuOptions {

    /** 10 CDEMO-ADMIN-OPT OCCURS 9 TIMES. */
    public static final int MAX_OCCURS = 9;

    /** 15 CDEMO-ADMIN-OPT-NUM PIC 9(02), CDEMO-ADMIN-OPT-NAME PIC X(35), CDEMO-ADMIN-OPT-PGMNAME PIC X(08). */
    public record AdminMenuOption(int num, String name, String pgmName) {
        public static final int NAME_LENGTH = 35;
        public static final int PGMNAME_LENGTH = 8;

        public AdminMenuOption {
            if (num < 0 || num > 99) {
                throw new IllegalArgumentException("CDEMO-ADMIN-OPT-NUM is PIC 9(02): " + num);
            }
            name = CobolFields.alnum(name, NAME_LENGTH);
            pgmName = CobolFields.alnum(pgmName, PGMNAME_LENGTH);
        }
    }

    private static final AdminMenuOptions STANDARD = new AdminMenuOptions(6, List.of(
            new AdminMenuOption(1, "User List (Security)", "COUSR00C"),
            new AdminMenuOption(2, "User Add (Security)", "COUSR01C"),
            new AdminMenuOption(3, "User Update (Security)", "COUSR02C"),
            new AdminMenuOption(4, "User Delete (Security)", "COUSR03C"),
            new AdminMenuOption(5, "Transaction Type List/Update (Db2)", "COTRTLIC"),
            new AdminMenuOption(6, "Transaction Type Maintenance (Db2)", "COTRTUPC")));

    private final int count;
    private final List<AdminMenuOption> options;

    /**
     * @param count   05 CDEMO-ADMIN-OPT-COUNT PIC 9(02)
     * @param options CDEMO-ADMIN-OPTIONS-DATA, at most {@link #MAX_OCCURS} entries
     */
    public AdminMenuOptions(int count, List<AdminMenuOption> options) {
        if (options.size() > MAX_OCCURS) {
            throw new IllegalArgumentException("CDEMO-ADMIN-OPT OCCURS " + MAX_OCCURS + " TIMES");
        }
        if (count < 0 || count > options.size()) {
            throw new IllegalArgumentException("CDEMO-ADMIN-OPT-COUNT " + count + " exceeds defined options");
        }
        this.count = count;
        this.options = List.copyOf(options);
    }

    /** The values compiled into COADM02Y (Db2 release: 6 options). */
    public static AdminMenuOptions standard() {
        return STANDARD;
    }

    public int count() {
        return count;
    }

    /** CDEMO-ADMIN-OPT(idx), 1-based. */
    public AdminMenuOption option(int idx) {
        return options.get(idx - 1);
    }

    public List<AdminMenuOption> options() {
        return options;
    }
}
