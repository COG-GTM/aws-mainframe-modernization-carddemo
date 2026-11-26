package com.carddemo.programs.comen01c.copybook;

import java.util.ArrayList;
import java.util.List;

/**
 * COMEN02Y - {@code 01 CARDDEMO-MAIN-MENU-OPTIONS}.
 *
 * <p>{@code CDEMO-MENU-OPT OCCURS 12 TIMES} redefines the VALUE data; only the first
 * {@code CDEMO-MENU-OPT-COUNT} (11) slots are initialised. The 12th slot has no VALUE in the copybook and
 * is modelled as blank.
 */
public final class MainMenuOptions {

    public static final int MAX_OPTIONS = 12;

    /** 15 CDEMO-MENU-OPT: NUM PIC 9(02), NAME PIC X(35), PGMNAME PIC X(08), USRTYPE PIC X(01). */
    public record MenuOption(int num, String name, String pgmName, String usrType) {
        public MenuOption {
            name = CobolText.move(name, 35);
            pgmName = CobolText.move(pgmName, 8);
            usrType = CobolText.move(usrType, 1);
        }

        public static final MenuOption BLANK = new MenuOption(0, "", "", "");

        public String numDisplay() {
            return CobolText.pic9(num, 2);
        }

        public boolean isAdminOnly() {
            return "A".equals(usrType);
        }
    }

    private final int count;
    private final List<MenuOption> options;

    public MainMenuOptions(int count, List<MenuOption> configured) {
        if (configured.size() > MAX_OPTIONS) {
            throw new IllegalArgumentException("CDEMO-MENU-OPT OCCURS 12 TIMES");
        }
        this.count = count % 100;
        List<MenuOption> slots = new ArrayList<>(configured);
        while (slots.size() < MAX_OPTIONS) {
            slots.add(MenuOption.BLANK);
        }
        this.options = List.copyOf(slots);
    }

    /** Values exactly as coded in app/cpy/COMEN02Y.cpy. */
    public static MainMenuOptions standard() {
        return new MainMenuOptions(11, List.of(
                new MenuOption(1, "Account View", "COACTVWC", "U"),
                new MenuOption(2, "Account Update", "COACTUPC", "U"),
                new MenuOption(3, "Credit Card List", "COCRDLIC", "U"),
                new MenuOption(4, "Credit Card View", "COCRDSLC", "U"),
                new MenuOption(5, "Credit Card Update", "COCRDUPC", "U"),
                new MenuOption(6, "Transaction List", "COTRN00C", "U"),
                new MenuOption(7, "Transaction View", "COTRN01C", "U"),
                new MenuOption(8, "Transaction Add", "COTRN02C", "U"),
                new MenuOption(9, "Transaction Reports", "CORPT00C", "U"),
                new MenuOption(10, "Bill Payment", "COBIL00C", "U"),
                new MenuOption(11, "Pending Authorization View", "COPAUS0C", "U")));
    }

    /** CDEMO-MENU-OPT-COUNT PIC 9(02). */
    public int count() {
        return count;
    }

    /**
     * CDEMO-MENU-OPT(idx), 1-based. Subscripts outside 1..12 have no defined content in COBOL (no SSRANGE);
     * they are returned as a blank slot.
     */
    public MenuOption option(int idx) {
        if (idx < 1 || idx > MAX_OPTIONS) {
            return MenuOption.BLANK;
        }
        return options.get(idx - 1);
    }
}
