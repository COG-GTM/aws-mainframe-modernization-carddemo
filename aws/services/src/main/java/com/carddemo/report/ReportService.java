package com.carddemo.report;

import com.carddemo.common.ApiException;
import com.carddemo.common.ErrorCode;
import com.carddemo.common.Text;
import com.carddemo.common.ValidationErrors;
import java.time.Clock;
import java.time.Instant;
import java.time.LocalDate;
import java.time.YearMonth;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.time.format.ResolverStyle;
import java.util.UUID;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

/** CORPT00C: derives the report date range and submits the report job. */
@Service
public class ReportService {

    static final String PROGRAM = "CORPT00C";
    private static final Logger LOG = LoggerFactory.getLogger(ReportService.class);
    private static final DateTimeFormatter STRICT_DATE = DateTimeFormatter.ofPattern("uuuu-MM-dd")
            .withResolverStyle(ResolverStyle.STRICT);

    private final ReportPublisher publisher;
    private final ReportStatusProvider statusProvider;
    private final Clock clock;

    public ReportService(ReportPublisher publisher, ReportStatusProvider statusProvider, Clock clock) {
        this.publisher = publisher;
        this.statusProvider = statusProvider;
        this.clock = clock;
    }

    public record SubmitRequest(String reportType, String startDate, String endDate) {
    }

    public record SubmitResponse(UUID requestId, String reportType, LocalDate startDate, LocalDate endDate,
            String message) {
    }

    public record StatusResponse(UUID requestId, ReportStatusProvider.Status status, String reportS3Key) {
    }

    public SubmitResponse submit(SubmitRequest request, String requestedBy) {
        String type = request == null ? "" : Text.upperTrim(request.reportType());
        LocalDate today = LocalDate.now(clock);
        LocalDate start;
        LocalDate end;
        String reportName;
        switch (type) {
            case "MONTHLY" -> {
                YearMonth month = YearMonth.from(today);
                start = month.atDay(1);
                end = month.atEndOfMonth();
                reportName = "Monthly";
            }
            case "YEARLY" -> {
                start = LocalDate.of(today.getYear(), 1, 1);
                end = LocalDate.of(today.getYear(), 12, 31);
                reportName = "Yearly";
            }
            case "CUSTOM" -> {
                ValidationErrors errors = new ValidationErrors(PROGRAM);
                start = parse(request.startDate());
                if (Text.isBlank(request.startDate())) {
                    errors.add("startDate", "Start Date can NOT be empty...");
                } else if (start == null) {
                    errors.add("startDate", "Start Date - Not a valid date...");
                }
                errors.throwIfAny();
                end = parse(request.endDate());
                if (Text.isBlank(request.endDate())) {
                    errors.add("endDate", "End Date can NOT be empty...");
                } else if (end == null) {
                    errors.add("endDate", "End Date - Not a valid date...");
                }
                errors.throwIfAny();
                reportName = "Custom";
            }
            default -> throw ApiException.validation(PROGRAM, "reportType",
                    "Select a report type to print report...");
        }
        UUID requestId = UUID.randomUUID();
        ReportRequest message = new ReportRequest("1", requestId, type, start, end, requestedBy, Instant.now(clock));
        try {
            publisher.publish(message);
        } catch (RuntimeException ex) {
            LOG.error("Report request {} could not be published", requestId, ex);
            throw new ApiException(ErrorCode.INTERNAL_ERROR, "Unable to Write TDQ (JOBS)...", PROGRAM);
        }
        return new SubmitResponse(requestId, type, start, end, reportName + " report submitted for printing ...");
    }

    public StatusResponse status(String requestIdIn) {
        UUID requestId;
        try {
            requestId = UUID.fromString(Text.trimToEmpty(requestIdIn));
        } catch (IllegalArgumentException ex) {
            throw ApiException.validation(PROGRAM, "requestId", "requestId must be a UUID");
        }
        ReportStatusProvider.ReportStatus status = statusProvider.status(requestId);
        return new StatusResponse(requestId, status.status(), status.reportS3Key());
    }

    private static LocalDate parse(String value) {
        if (Text.isBlank(value)) {
            return null;
        }
        try {
            return LocalDate.parse(value.strip(), STRICT_DATE);
        } catch (DateTimeParseException ex) {
            return null;
        }
    }
}
