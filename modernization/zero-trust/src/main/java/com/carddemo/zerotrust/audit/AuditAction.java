package com.carddemo.zerotrust.audit;

/** Action names written to the audit chain. */
public final class AuditAction {

    public static final String AUTHZ_DECISION = "AUTHZ_DECISION";
    public static final String JOB_STARTED = "JOB_STARTED";
    public static final String TRANSACTION_POSTED = "TRANSACTION_POSTED";
    public static final String TRANSACTION_REJECTED = "TRANSACTION_REJECTED";
    public static final String TRANSACTION_WRITE_FAILED = "TRANSACTION_WRITE_FAILED";
    public static final String JOB_COMPLETED = "JOB_COMPLETED";
    public static final String CUTOVER_SUBMITTED = "CUTOVER_SUBMITTED";
    public static final String CUTOVER_APPROVED = "CUTOVER_APPROVED";
    public static final String REJECTS_VIEWED = "REJECTS_VIEWED";
    public static final String AUDIT_READ = "AUDIT_READ";

    private AuditAction() {
    }
}
