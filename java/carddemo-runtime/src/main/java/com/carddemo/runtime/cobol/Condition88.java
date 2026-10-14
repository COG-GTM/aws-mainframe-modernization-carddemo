package com.carddemo.runtime.cobol;

import java.math.BigDecimal;
import java.util.List;

/**
 * A level-88 condition name over an alphanumeric or numeric item. {@link #isTrue} tests the condition;
 * {@link #set} implements {@code SET condition TO TRUE} (moves the first listed value).
 */
public final class Condition88 {

    private final String name;
    private final CobolField field;
    private final List<String> alphaValues;
    private final List<BigDecimal[]> numericRanges;

    private Condition88(String name, CobolField field, List<String> alphaValues, List<BigDecimal[]> numericRanges) {
        this.name = name;
        this.field = field;
        this.alphaValues = alphaValues;
        this.numericRanges = numericRanges;
    }

    /** {@code 88 name VALUE 'a' 'b' ...} on a PIC X item. */
    public static Condition88 of(String name, AlphanumericField field, String... values) {
        return new Condition88(name, field, List.of(values), List.of());
    }

    /** {@code 88 name VALUE n1 n2 ...} on a numeric item. */
    public static Condition88 of(String name, NumericField field, long... values) {
        BigDecimal[][] ranges = new BigDecimal[values.length][];
        for (int i = 0; i < values.length; i++) {
            ranges[i] = new BigDecimal[] {BigDecimal.valueOf(values[i]), BigDecimal.valueOf(values[i])};
        }
        return new Condition88(name, field, List.of(), List.of(ranges));
    }

    /** {@code 88 name VALUE low THRU high} on a numeric item. */
    public static Condition88 range(String name, NumericField field, BigDecimal low, BigDecimal high) {
        List<BigDecimal[]> ranges = List.<BigDecimal[]>of(new BigDecimal[] {low, high});
        return new Condition88(name, field, List.of(), ranges);
    }

    public String name() {
        return name;
    }

    public boolean isTrue(CobolRecord record) {
        if (field instanceof AlphanumericField a) {
            String v = a.get(record);
            return alphaValues.stream().anyMatch(candidate -> PicX.equal(v, candidate));
        }
        NumericField n = (NumericField) field;
        if (!n.isNumeric(record)) {
            return false;
        }
        BigDecimal v = n.get(record);
        return numericRanges.stream().anyMatch(r -> v.compareTo(r[0]) >= 0 && v.compareTo(r[1]) <= 0);
    }

    public void set(CobolRecord record) {
        if (field instanceof AlphanumericField a) {
            a.set(record, alphaValues.getFirst());
        } else {
            ((NumericField) field).set(record, numericRanges.getFirst()[0]);
        }
    }
}
