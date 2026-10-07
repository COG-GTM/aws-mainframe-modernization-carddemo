package com.carddemo.user.menu;

import java.util.List;

/**
 * A menu program, its map and its option table.
 *
 * @param key            path segment of the menu endpoints ({@code main}, {@code admin})
 * @param programId      {@code WS-PGMNAME}
 * @param tranId         {@code WS-TRANID}
 * @param mapset         BMS mapset
 * @param map            BMS map
 * @param optionFields   number of {@code OPTN0nn} fields on the map (12 on both maps)
 * @param checksUserType COMEN01C R-9 (admin-only options for a user); the admin menu has no such check
 * @param options        the table; its size is {@code CDEMO-MENU-OPT-COUNT} / {@code CDEMO-ADMIN-OPT-COUNT}
 */
public record MenuDefinition(String key, String programId, String tranId, String mapset, String map,
        int optionFields, boolean checksUserType, List<MenuOption> options) {

    public MenuDefinition {
        options = List.copyOf(options);
        if (options.size() > optionFields) {
            throw new IllegalArgumentException(programId + ": " + options.size() + " options for " + optionFields
                    + " option fields");
        }
    }

    public int optionCount() {
        return options.size();
    }

    /** The option for a number in 1..{@link #optionCount()}. */
    public MenuOption option(int number) {
        return options.get(number - 1);
    }

    /** {@code OPTN001O}..{@code OPTN0nnO} as sent: one label per option, blank for the unused fields. */
    public List<String> optionLines() {
        String[] lines = new String[optionFields];
        for (int i = 0; i < optionFields; i++) {
            lines[i] = i < options.size() ? options.get(i).label() : "";
        }
        return List.of(lines);
    }
}
