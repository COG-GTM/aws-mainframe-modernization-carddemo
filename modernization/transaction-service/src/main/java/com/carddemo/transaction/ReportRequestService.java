package com.carddemo.transaction;

import com.carddemo.common.domain.ReportRequest;
import com.carddemo.common.domain.ReportRequestRepository;
import com.carddemo.common.error.ApiException;
import com.carddemo.common.security.CurrentUser;
import java.time.Clock;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.YearMonth;
import java.util.Optional;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/** CORPT00C (CR00): queues a TRANREPT run (monthly, yearly or custom range) for batch-service. */
@Service
public class ReportRequestService {

    private final ReportRequestRepository requests;
    private final Clock clock;

    public ReportRequestService(ReportRequestRepository requests, Optional<Clock> clock) {
        this.requests = requests;
        this.clock = clock.orElse(Clock.systemDefaultZone());
    }

    public record ReportCommand(String reportType, String startDate, String endDate, String confirm) {
    }

    @Transactional
    public ReportRequest submit(ReportCommand c) {
        String type = c.reportType() == null ? "" : c.reportType().trim().toUpperCase();
        LocalDate today = LocalDate.now(clock);
        LocalDate start;
        LocalDate end;
        switch (type) {
            case "MONTHLY" -> {
                YearMonth ym = YearMonth.from(today);
                start = ym.atDay(1);
                end = ym.atEndOfMonth();
            }
            case "YEARLY" -> {
                start = LocalDate.of(today.getYear(), 1, 1);
                end = LocalDate.of(today.getYear(), 12, 31);
            }
            case "CUSTOM" -> {
                start = parse(c.startDate(), "Start Date");
                end = parse(c.endDate(), "End Date");
            }
            default -> throw ApiException.badRequest("VALIDATION", "Select a report type to print report...");
        }
        if (c.confirm() == null || !c.confirm().trim().equalsIgnoreCase("Y")) {
            throw ApiException.badRequest("CONFIRM",
                    "Please confirm to print the " + type.charAt(0) + type.substring(1).toLowerCase() + " report...");
        }
        return requests.save(new ReportRequest(type, start, end, CurrentUser.get().userId(),
                LocalDateTime.now(clock)));
    }

    private static LocalDate parse(String v, String label) {
        if (v == null || v.isBlank()) {
            throw ApiException.badRequest("VALIDATION", label + " can NOT be empty...");
        }
        try {
            return LocalDate.parse(v.trim());
        } catch (java.time.format.DateTimeParseException e) {
            throw ApiException.badRequest("VALIDATION", label + " - Not a valid date...");
        }
    }

    @Transactional(readOnly = true)
    public ReportRequest get(long id) {
        return requests.findById(id).orElseThrow(() -> ApiException.notFound("REPORT_NOT_FOUND", "Report not found"));
    }
}
