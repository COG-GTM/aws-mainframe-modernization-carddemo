package com.carddemo.interest.repository;

import com.carddemo.interest.domain.AccountId;
import com.carddemo.interest.domain.CardXref;

import java.util.Optional;

/**
 * Access to the card cross-reference ({@code XREFFILE} DD) through its account-id alternate index
 * ({@code AWS.M2.CARDDEMO.CARDXREF.VSAM.AIX.PATH}).
 *
 * <p>Replaces {@code READ XREF-FILE ... KEY IS FD-XREF-ACCT-ID} in {@code 1110-GET-XREF-DATA}
 * ({@code app/cbl/CBACT04C.cbl:393-398}). An alternate-key read with duplicates returns the first
 * record in primary-key (card number) order, which {@link #findByAccountId} reproduces.
 */
public interface CardXrefRepository {

    Optional<CardXref> findByAccountId(AccountId accountId);
}
