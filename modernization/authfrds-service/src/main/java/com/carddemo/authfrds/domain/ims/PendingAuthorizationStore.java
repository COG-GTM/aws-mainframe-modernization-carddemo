package com.carddemo.authfrds.domain.ims;

import com.carddemo.authfrds.domain.AuthKey;
import com.carddemo.authfrds.domain.AuthorizationDetail;
import java.util.List;
import java.util.Optional;

/**
 * Port for the IMS PAUT database (DBPAUTP0, PSB PSBPAUTB) as COPAUS1C uses it. IMS is NOT implemented by this
 * service: the only adapter is an in-memory stub seeded from the legacy IMS unload. DL/I failures surface as
 * {@link ImsStatusException}; "not found" (GE / GB) surfaces as an empty Optional.
 */
public interface PendingAuthorizationStore {

    /** GU PAUTSUM0 WHERE ACCNTID = :acctId. */
    Optional<PendingAuthorizationSummary> findSummary(long acctId);

    /** GNP PAUTDTL1 WHERE PAUT9CTS = :key. */
    Optional<AuthorizationDetail> findDetail(long acctId, AuthKey key);

    /** GNP PAUTDTL1 (unqualified) after the current key - PF8 next authorization. */
    Optional<AuthorizationDetail> findNextDetail(long acctId, AuthKey key);

    /** All PAUTDTL1 children of the account in PAUT9CTS order (what COPAUS0C pages through). */
    List<AuthorizationDetail> listDetails(long acctId);

    /** REPL PAUTDTL1 FROM (PENDING-AUTH-DETAILS). */
    void replaceDetail(long acctId, AuthorizationDetail detail);
}
