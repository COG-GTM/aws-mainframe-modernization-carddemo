package com.carddemo.transaction.batch;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.IdClass;
import jakarta.persistence.Table;

import java.io.Serializable;

/**
 * {@code batch.posttran_reject}: one staged DALYREJS image (430 raw bytes in the input encoding), keyed by the
 * JobInstance and the 1-based input record number, so a restart of the instance keeps the rejects already committed.
 */
@Entity
@Table(schema = "batch", name = "posttran_reject")
@IdClass(PosttranReject.Key.class)
public class PosttranReject {

    public record Key(Long jobInstanceId, Long inputSeq) implements Serializable {
        public Key() {
            this(null, null);
        }
    }

    @Id
    @Column(name = "job_instance_id")
    private Long jobInstanceId;

    @Id
    @Column(name = "input_seq")
    private Long inputSeq;

    @Column(name = "record", nullable = false, columnDefinition = "bytea")
    private byte[] record;

    protected PosttranReject() {
    }

    public PosttranReject(long jobInstanceId, long inputSeq, byte[] record) {
        this.jobInstanceId = jobInstanceId;
        this.inputSeq = inputSeq;
        this.record = record.clone();
    }

    public long getJobInstanceId() { return jobInstanceId; }
    public long getInputSeq() { return inputSeq; }
    public byte[] getRecord() { return record.clone(); }
}
