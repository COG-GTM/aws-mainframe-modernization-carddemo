package com.carddemo.programs.cbstm03b.copybook;

/**
 * TRNXFILE-STATUS / XREFFILE-STATUS / CUSTFILE-STATUS / ACCTFILE-STATUS.
 * <pre>
 * 01 xxxxFILE-STATUS.
 *    05 xxxxFILE-STAT1 PIC X.
 *    05 xxxxFILE-STAT2 PIC X.
 * </pre>
 */
public record FileStatus(char stat1, char stat2) {

    /** WORKING-STORAGE item with no VALUE clause: spaces until the first I/O statement. */
    public static final FileStatus INITIAL = of("  ");
    public static final FileStatus SUCCESS = of("00");
    public static final FileStatus END_OF_FILE = of("10");
    public static final FileStatus RECORD_NOT_FOUND = of("23");
    public static final FileStatus FILE_NOT_FOUND = of("35");
    public static final FileStatus ALREADY_OPEN = of("41");
    public static final FileStatus NOT_OPEN_ON_CLOSE = of("42");
    public static final FileStatus READ_AFTER_END = of("46");
    public static final FileStatus NOT_OPEN_FOR_READ = of("47");

    public static FileStatus of(String code) {
        if (code == null || code.length() != 2) {
            throw new IllegalArgumentException("file status must be 2 characters: " + code);
        }
        return new FileStatus(code.charAt(0), code.charAt(1));
    }

    public String code() {
        return "" + stat1 + stat2;
    }

    /** Status key 1 = '0': the I/O statement completed successfully (READ ... INTO is performed). */
    public boolean isSuccessful() {
        return stat1 == '0';
    }

    @Override
    public String toString() {
        return code();
    }
}
