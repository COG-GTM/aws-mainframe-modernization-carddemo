package com.carddemo.user.menu;

import com.carddemo.common.online.InstalledPrograms;
import com.carddemo.common.online.MessageColor;
import com.carddemo.user.UserType;
import java.util.Optional;
import org.springframework.stereotype.Service;

/**
 * {@code PROCESS-ENTER-KEY} of {@code COMEN01C} and {@code COADM01C}: option normalisation (R-7), validation
 * (R-8), the admin-only check of the main menu (COMEN01C R-9) and the dispatch decision (COMEN01C R-10..R-12,
 * COADM01C R-9/R-10).
 */
@Service
public class MenuService {

    public static final String MSG_INVALID_OPTION = "Please enter a valid option number...";
    /** COBOL literal {@code 'No access - Admin Only option... '}, right-trimmed (ADR-0003). */
    public static final String MSG_ADMIN_ONLY = "No access - Admin Only option...";
    public static final String MSG_ADMIN_NOT_INSTALLED = "This option is not installed ...";

    /** The main-menu row checked with {@code EXEC CICS INQUIRE PROGRAM} before the XCTL (COMEN01C R-10). */
    static final String INQUIRED_PROGRAM = "COPAUS0C";

    private final MenuCatalog catalog;
    private final InstalledPrograms installed;

    public MenuService(MenuCatalog catalog, InstalledPrograms installed) {
        this.catalog = catalog;
        this.installed = installed;
    }

    public MenuCatalog catalog() {
        return catalog;
    }

    /**
     * R-7: scan {@code OPTIONI} from the right to the last non-space, keep {@code OPTIONI(1:idx)}, replace every
     * space by {@code '0'} and move it right-justified into {@code WS-OPTION PIC 9(02)}. Returns the two characters
     * that land in {@code WS-OPTION} ({@code "1 "} → {@code "01"}, {@code " 1"} → {@code "01"}, {@code "7"} →
     * {@code "07"}); they are not necessarily digits.
     */
    public static String normalizeOption(String optionInput) {
        String raw = optionInput == null ? "" : optionInput;
        String field = raw.length() >= 2 ? raw.substring(0, 2) : raw + " ".repeat(2 - raw.length());
        int idx = 2;
        while (idx > 1 && field.charAt(idx - 1) == ' ') {
            idx--;
        }
        String kept = field.substring(0, idx).replace(' ', '0');
        return kept.length() == 2 ? kept : "0" + kept;
    }

    /** R-8 numeric test plus range check; empty when the option is not a valid number of {@code menu}. */
    static Optional<Integer> optionNumber(String normalized, MenuDefinition menu) {
        if (normalized.length() != 2 || !Character.isDigit(normalized.charAt(0))
                || !Character.isDigit(normalized.charAt(1))) {
            return Optional.empty();
        }
        int n = Integer.parseInt(normalized);
        return n == 0 || n > menu.optionCount() ? Optional.empty() : Optional.of(n);
    }

    public MenuSelection select(MenuDefinition menu, UserType userType, String optionInput) {
        String option = normalizeOption(optionInput);
        Optional<Integer> number = optionNumber(option, menu);
        if (number.isEmpty()) {
            return new MenuSelection.Rejected(option, MenuRejection.INVALID_OPTION, MSG_INVALID_OPTION);
        }
        MenuOption target = menu.option(number.get());
        if (menu.checksUserType() && userType == UserType.USER && target.adminOnly()) {
            return new MenuSelection.Rejected(option, MenuRejection.ADMIN_ONLY, MSG_ADMIN_ONLY);
        }
        return menu.checksUserType() ? dispatchMain(menu, option, target) : dispatchAdmin(menu, option, target);
    }

    /** COMEN01C {@code EVALUATE TRUE}: R-10 COPAUS0C, R-11 DUMMY, R-12 other. */
    private MenuSelection dispatchMain(MenuDefinition menu, String option, MenuOption target) {
        if (INQUIRED_PROGRAM.equals(target.programId())) {
            Optional<String> tranId = installed.tranIdOf(target.programId());
            if (tranId.isPresent()) {
                return transfer(menu, option, target, tranId.get());
            }
            return new MenuSelection.Info(option,
                    "This option " + delimitedByTwoSpaces(target.name()) + " is not installed...", MessageColor.RED);
        }
        if (target.dummy()) {
            return new MenuSelection.Info(option,
                    "This option " + delimitedBySpace(target.name()) + "is coming soon ...", MessageColor.GREEN);
        }
        return transfer(menu, option, target, installed.tranIdOf(target.programId()).orElse(null));
    }

    /**
     * COADM01C R-9/R-10: XCTL unless DUMMY; a DUMMY row, or a program that is not installed ({@code HANDLE
     * CONDITION PGMIDERR(PGMIDERR-ERR-PARA)} in MAIN-PARA), re-sends the menu with the same green message.
     */
    private MenuSelection dispatchAdmin(MenuDefinition menu, String option, MenuOption target) {
        Optional<String> tranId = installed.tranIdOf(target.programId());
        if (target.dummy() || tranId.isEmpty()) {
            return new MenuSelection.Info(option, MSG_ADMIN_NOT_INSTALLED, MessageColor.GREEN);
        }
        return transfer(menu, option, target, tranId.get());
    }

    private static MenuSelection transfer(MenuDefinition menu, String option, MenuOption target, String tranId) {
        return new MenuSelection.Transfer(option, target, tranId, menu.programId(), menu.tranId());
    }

    /** {@code STRING name DELIMITED BY '  '} on the 35-byte, space-padded name. */
    static String delimitedByTwoSpaces(String name) {
        String padded = name + "  ";
        return padded.substring(0, padded.indexOf("  "));
    }

    /** {@code STRING name DELIMITED BY SPACE}: the first word. */
    static String delimitedBySpace(String name) {
        int space = name.indexOf(' ');
        return space < 0 ? name : name.substring(0, space);
    }
}
