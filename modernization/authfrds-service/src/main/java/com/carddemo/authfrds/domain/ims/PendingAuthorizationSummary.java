package com.carddemo.authfrds.domain.ims;

/** IMS root segment PAUTSUM0 (copybook CIPAUSMY), only the fields the fraud flow uses. */
public record PendingAuthorizationSummary(long acctId, long custId) {
}
