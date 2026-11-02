package com.carddemo.common.record;

/**
 * How records are laid out in a file.
 *
 * @param organization       fixed-length blocks ({@code RECFM=F}, the EBCDIC {@code .PS} files) or one
 *                           record per line (the {@code app/data/ASCII} extracts)
 * @param lineEnding         terminator written after each line-sequential record ({@code \n} or {@code \r\n});
 *                           the reader accepts both
 * @param trimTrailingSpaces whether the writer drops trailing spaces of a line-sequential record (as
 *                           {@code cardxref.txt} does); the reader pads short lines back with spaces
 */
public record RecordFormat(Organization organization, String lineEnding, boolean trimTrailingSpaces) {

    public enum Organization { FIXED, LINE_SEQUENTIAL }

    public static final RecordFormat FIXED = new RecordFormat(Organization.FIXED, "", false);

    public static final RecordFormat LINE_SEQUENTIAL = new RecordFormat(Organization.LINE_SEQUENTIAL, "\n", false);

    public RecordFormat {
        if (organization == Organization.LINE_SEQUENTIAL && !lineEnding.equals("\n") && !lineEnding.equals("\r\n")) {
            throw new IllegalArgumentException("Line ending must be \\n or \\r\\n");
        }
    }

    public RecordFormat withCrlf() {
        return new RecordFormat(organization, "\r\n", trimTrailingSpaces);
    }

    public RecordFormat trimmingTrailingSpaces() {
        return new RecordFormat(organization, lineEnding, true);
    }
}
