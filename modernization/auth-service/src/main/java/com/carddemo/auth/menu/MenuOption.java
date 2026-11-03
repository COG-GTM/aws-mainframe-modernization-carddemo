package com.carddemo.auth.menu;

/**
 * One row of COMEN02Y / COADM02Y: {@code number} = option index, {@code name} = OPTION-NAME,
 * {@code program} = PGMNAME, {@code adminOnly} = USRTYPE {@code 'A'}. {@code available} is false
 * for programs this migration does not provide (DB2/IMS/MQ extensions); {@code endpoint} is the REST operation that replaces the XCTL target, or null.
 */
public record MenuOption(int number, String name, String program, boolean adminOnly, boolean available,
                         String endpoint) {
}
