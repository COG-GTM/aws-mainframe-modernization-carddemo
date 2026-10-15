package com.carddemo.common.domain;

import jakarta.persistence.*;
import java.time.LocalDate;
import java.time.LocalDateTime;

/** Table report_request; replaces CORPT00C submitting the TRANREPT job through the CICS internal reader. */
@Entity
@Table(name = "report_request")
public class ReportRequest {

    public static final String SUBMITTED = "SUBMITTED";
    public static final String COMPLETED = "COMPLETED";

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "report_type")
    private String reportType;

    @Column(name = "start_date")
    private LocalDate startDate;

    @Column(name = "end_date")
    private LocalDate endDate;

    @Column(name = "requested_by")
    private String requestedBy;

    @Column(name = "status")
    private String status;

    @Column(name = "requested_at")
    private LocalDateTime requestedAt;

    @Column(name = "completed_at")
    private LocalDateTime completedAt;

    @Column(name = "output")
    private String output;

    protected ReportRequest() {
    }

    public ReportRequest(String reportType, LocalDate startDate, LocalDate endDate, String requestedBy,
                         LocalDateTime requestedAt) {
        this.reportType = reportType;
        this.startDate = startDate;
        this.endDate = endDate;
        this.requestedBy = requestedBy;
        this.requestedAt = requestedAt;
        this.status = SUBMITTED;
    }

    public void complete(String output, LocalDateTime completedAt) {
        this.output = output;
        this.completedAt = completedAt;
        this.status = COMPLETED;
    }

    public Long getId() {
        return id;
    }

    public String getReportType() {
        return reportType;
    }

    public LocalDate getStartDate() {
        return startDate;
    }

    public LocalDate getEndDate() {
        return endDate;
    }

    public String getRequestedBy() {
        return requestedBy;
    }

    public String getStatus() {
        return status;
    }

    public LocalDateTime getRequestedAt() {
        return requestedAt;
    }

    public LocalDateTime getCompletedAt() {
        return completedAt;
    }

    public String getOutput() {
        return output;
    }
}
