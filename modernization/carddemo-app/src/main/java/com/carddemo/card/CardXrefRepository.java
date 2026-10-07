package com.carddemo.card;

import java.util.List;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;

/**
 * CARDXREF access paths: {@code READ} by XREF-CARD-NUM (COTRN02C, CBTRN01C/02C), the CXACAIX read by XREF-ACCT-ID
 * (COACTVWC, COACTUPC, COBIL00C, COTRN02C, CBACT04C) on index {@code card_xref_acct_id_ix (acct_id, card_num)}, and
 * the sequential read in key order (CBACT03C, CBSTM03A, CBEXPORT).
 */
public interface CardXrefRepository extends JpaRepository<CardXref, String> {

    List<CardXref> findAllByOrderByCardNumAsc();

    /** CXACAIX {@code READ} by XREF-ACCT-ID: the first cross-reference of the account (lowest card number). */
    Optional<CardXref> findFirstByAcctIdOrderByCardNumAsc(long acctId);

    /** Every card of an account in CXACAIX order ({@code READNEXT} while the alternate key is equal). */
    List<CardXref> findByAcctIdOrderByCardNumAsc(long acctId);
}
