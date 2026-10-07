package com.carddemo.batch;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import java.time.LocalDate;
import java.time.OffsetDateTime;
import org.hibernate.annotations.Generated;
import org.hibernate.annotations.Immutable;
import org.hibernate.generator.EventType;

/**
 * One GDG generation written as a dated file (ADR-0012), table {@code batch_output_file}. Not a VSAM dataset, so
 * there is no copybook or fixed-width mapper: {@code (0)} is the newest row per {@code gdg_base}, {@code (-1)} the one
 * before.
 */
@Entity
@Immutable
@Table(name = "batch_output_file")
public class BatchOutputFile {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "output_file_id")
    private Long outputFileId;

    /** GDG base without the {@code AWS.M2.CARDDEMO.} prefix, e.g. {@code TRANREPT}. */
    @Column(name = "gdg_base")
    private String gdgBase;

    @Column(name = "business_date")
    private LocalDate businessDate;

    @Column(name = "job_execution_id")
    private long jobExecutionId;

    @Column(name = "file_path")
    private String filePath;

    @Column(name = "record_count")
    private long recordCount;

    @Column(name = "sha256")
    private String sha256;

    @Generated(event = EventType.INSERT)
    @Column(name = "created_at", insertable = false, updatable = false)
    private OffsetDateTime createdAt;

    protected BatchOutputFile() {
    }

    public BatchOutputFile(String gdgBase, LocalDate businessDate, long jobExecutionId, String filePath,
                           long recordCount, String sha256) {
        this.gdgBase = gdgBase;
        this.businessDate = businessDate;
        this.jobExecutionId = jobExecutionId;
        this.filePath = filePath;
        this.recordCount = recordCount;
        this.sha256 = sha256;
    }

    public Long getOutputFileId() {
        return outputFileId;
    }

    public String getGdgBase() {
        return gdgBase;
    }

    public LocalDate getBusinessDate() {
        return businessDate;
    }

    public long getJobExecutionId() {
        return jobExecutionId;
    }

    public String getFilePath() {
        return filePath;
    }

    public long getRecordCount() {
        return recordCount;
    }

    public String getSha256() {
        return sha256;
    }

    public OffsetDateTime getCreatedAt() {
        return createdAt;
    }
}
