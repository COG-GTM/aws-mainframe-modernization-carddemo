package com.carddemo.user.menu;

import com.carddemo.user.UserType;

/**
 * One row of an option table ({@code CDEMO-MENU-OPT} of {@code COMEN02Y}, {@code CDEMO-ADMIN-OPT} of
 * {@code COADM02Y}).
 *
 * @param number    {@code -NUM PIC 9(02)}
 * @param name      {@code -NAME PIC X(35)}, right-trimmed
 * @param programId {@code -PGMNAME PIC X(08)}
 * @param userType  {@code CDEMO-MENU-OPT-USRTYPE}; {@code null} for the admin table, which has no such column
 */
public record MenuOption(int number, String name, String programId, UserType userType) {

    /** {@code BUILD-MENU-OPTIONS}: {@code OPTN0nnO} = {@code <NN>. <name>} (COMEN01C R-14, COADM01C R-13). */
    public String label() {
        return "%02d. %s".formatted(number, name);
    }

    /** {@code CDEMO-MENU-OPT-USRTYPE = 'A'}. */
    public boolean adminOnly() {
        return userType == UserType.ADMIN;
    }

    /** {@code PGMNAME(1:5) = 'DUMMY'}: a placeholder row. */
    public boolean dummy() {
        return programId.startsWith("DUMMY");
    }
}
