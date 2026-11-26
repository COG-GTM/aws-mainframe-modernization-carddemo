package com.carddemo.programs.codate01;

/** COBOL MOVE semantics for the field kinds CODATE01 uses. */
public final class CobolFields {

    private CobolFields() {
    }

    /** MOVE to PIC X(n): left-justify, pad with spaces, truncate on the right. */
    public static String alnum(String value, int length) {
        String v = value == null ? "" : value;
        if (v.length() >= length) {
            return v.substring(0, length);
        }
        return v + " ".repeat(length - v.length());
    }

    /** MOVE of a signed binary item to unsigned PIC 9(n): sign dropped, high-order digits truncated. */
    public static String unsignedDisplay(long value, int digits) {
        long abs = Math.abs(value);
        long mod = 1;
        for (int i = 0; i < digits; i++) {
            mod *= 10;
        }
        return String.format("%0" + digits + "d", abs % mod);
    }

    /** STRING ... DELIMITED BY SIZE INTO target: overlays from position 1, stops at target length, keeps the rest. */
    public static String stringInto(String target, String... parts) {
        StringBuilder sb = new StringBuilder();
        for (String p : parts) {
            sb.append(p);
        }
        int len = target.length();
        if (sb.length() >= len) {
            return sb.substring(0, len);
        }
        return sb + target.substring(sb.length());
    }

    /** MOVE source TO target where target is longer or shorter (group/alnum move). */
    public static String overlay(String target, String data) {
        int len = target.length();
        if (data.length() >= len) {
            return data.substring(0, len);
        }
        return data + target.substring(data.length());
    }

    public static String lowValues(int length) {
        return "\0".repeat(length);
    }
}
