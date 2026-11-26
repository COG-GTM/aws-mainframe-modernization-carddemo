package com.carddemo.programs.cbstm03a.support;

/**
 * The COBOL {@code STRING ... DELIMITED BY ... INTO target END-STRING} statement.
 * The receiving field is not cleared first (bytes past the last transferred one keep
 * their previous content) and transfer silently stops when the receiver is full
 * (no ON OVERFLOW clause is coded in CBSTM03A).
 */
public final class CobolString {

    private final char[] target;
    private int pointer;

    private CobolString(String currentTarget) {
        this.target = currentTarget.toCharArray();
    }

    public static CobolString into(String currentTarget) {
        return new CobolString(currentTarget);
    }

    /** Sending item {@code DELIMITED BY SIZE}. */
    public CobolString bySize(String source) {
        return transfer(source);
    }

    /** Sending item {@code DELIMITED BY delimiter}: up to (not incl.) its first occurrence. */
    public CobolString delimitedBy(String source, String delimiter) {
        int idx = source.indexOf(delimiter);
        return transfer(idx >= 0 ? source.substring(0, idx) : source);
    }

    private CobolString transfer(String part) {
        for (int i = 0; i < part.length() && pointer < target.length; i++) {
            target[pointer++] = part.charAt(i);
        }
        return this;
    }

    public String result() {
        return new String(target);
    }
}
