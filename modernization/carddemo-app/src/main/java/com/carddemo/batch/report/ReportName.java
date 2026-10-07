package com.carddemo.batch.report;

import java.util.Locale;
import java.util.Optional;

/** {@code WS-REPORT-NAME} of CORPT00C: which selector field ({@code MONTHLY}/{@code YEARLY}/{@code CUSTOM}) was set. */
public enum ReportName {
    MONTHLY("Monthly"), YEARLY("Yearly"), CUSTOM("Custom");

    private final String label;

    ReportName(String label) {
        this.label = label;
    }

    /** The text in the program's messages, e.g. {@code Monthly report submitted for printing ...}. */
    public String label() {
        return label;
    }

    /** {@code MONTHLY}/{@code Monthly}/{@code m}..., case-insensitive; empty when it names no report. */
    public static Optional<ReportName> of(String typed) {
        String text = typed == null ? "" : typed.strip().toUpperCase(Locale.ROOT);
        if (text.isEmpty()) {
            return Optional.empty();
        }
        for (ReportName name : values()) {
            if (name.name().equals(text) || name.name().substring(0, 1).equals(text)) {
                return Optional.of(name);
            }
        }
        return Optional.empty();
    }

    public static ReportName ofLabel(String label) {
        for (ReportName name : values()) {
            if (name.label.equals(label)) {
                return name;
            }
        }
        throw new IllegalArgumentException("unknown report name " + label);
    }
}
