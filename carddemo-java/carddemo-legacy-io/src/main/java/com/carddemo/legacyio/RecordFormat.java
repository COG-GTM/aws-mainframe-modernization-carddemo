package com.carddemo.legacyio;

/** How records follow each other in a file. */
public enum RecordFormat {
    /** Fixed-length records back to back with no separators: RECFM=FB datasets ({@code app/data/EBCDIC}). */
    FIXED,
    /** One record per line ({@code ORGANIZATION LINE SEQUENTIAL}, {@code app/data/ASCII}). */
    LINE_SEQUENTIAL;

    /** The format the CardDemo sample files use for an encoding. */
    public static RecordFormat defaultFor(Encoding encoding) {
        return encoding == Encoding.EBCDIC ? FIXED : LINE_SEQUENTIAL;
    }
}
