package com.carddemo.batch.report;

import com.carddemo.common.InvalidRequestException;
import com.carddemo.common.online.ScreenInput;
import java.util.Locale;
import org.springframework.stereotype.Service;

/**
 * CORPT00C ENTER: {@link TransactionReportEdits} builds the window, then {@code SUBMIT-JOB-TO-INTRDR} asks for
 * confirmation: blank → {@code Please confirm to print the <name> report...} (R-15), {@code N} → screen cleared
 * (R-17), {@code Y} → submitted (R-16/R-18), anything else → {@code "<value>" is not a valid value to confirm...}.
 */
@Service
public class TransactionReportService {

    public static final String CONFIRM_FIELD = "confirm";

    /** What ENTER did. */
    public enum State { VALIDATED, CANCELLED, SUBMITTED }

    /** {@code window} is null when the screen was cleared; {@code execution} only when SUBMITTED. */
    public record Outcome(State state, ReportWindow window, ReportExecution execution, String message) {
    }

    private final TransactionReportEdits edits;
    private final TransactionReportLauncher launcher;

    public TransactionReportService(TransactionReportEdits edits, TransactionReportLauncher launcher) {
        this.edits = edits;
        this.launcher = launcher;
    }

    public Outcome request(String reportType, ReportDate start, ReportDate end, String confirm, String requestedBy) {
        ReportWindow window = edits.edit(reportType, start, end);
        String answer = ScreenInput.isSpacesOrLowValues(confirm) ? "" : confirm.strip().toUpperCase(Locale.ROOT);
        return switch (answer) {
            case "" -> new Outcome(State.VALIDATED, window, null, pleaseConfirm(window.name()));
            case "N" -> new Outcome(State.CANCELLED, null, null, "");
            case "Y" -> new Outcome(State.SUBMITTED, window, launcher.submit(window, requestedBy),
                    submitted(window.name()));
            default -> throw new InvalidRequestException(CONFIRM_FIELD, invalidConfirm(confirm));
        };
    }

    /** R-15. */
    public static String pleaseConfirm(ReportName name) {
        return "Please confirm to print the " + name.label() + " report...";
    }

    /** R-14. */
    public static String submitted(ReportName name) {
        return name.label() + " report submitted for printing ...";
    }

    /** R-17: {@code "<value>" is not a valid value to confirm...}, value delimited by space. */
    public static String invalidConfirm(String confirm) {
        String value = confirm.strip();
        int space = value.indexOf(' ');
        return "\"" + (space < 0 ? value : value.substring(0, space)) + "\" is not a valid value to confirm...";
    }
}
