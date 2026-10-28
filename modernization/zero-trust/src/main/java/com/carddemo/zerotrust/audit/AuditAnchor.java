package com.carddemo.zerotrust.audit;

/**
 * The current chain head — sequence number and hash of the last entry. Holding this
 * externally lets the verifier detect tail truncation and full-log replacement.
 */
public record AuditAnchor(long seq, String hash) {
}
