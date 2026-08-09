package com.carddemo.posting.repository;

import com.carddemo.posting.domain.CardNumber;
import com.carddemo.posting.domain.CardXref;

import java.util.Optional;

/**
 * The card cross-reference file ({@code XREFFILE} DD,
 * {@code AWS.M2.CARDDEMO.CARDXREF.VSAM.KSDS}, {@code app/jcl/POSTTRAN.jcl:32-33}).
 *
 * <p>Replaces the keyed {@code READ XREF-FILE} of {@code 1500-A-LOOKUP-XREF}
 * ({@code app/cbl/CBTRN02C.cbl:380-392}). A miss is an ordinary business outcome here, not an
 * error, which is why it is an {@link Optional} rather than an exception.
 */
public interface CardXrefRepository {

    Optional<CardXref> findByCardNumber(CardNumber cardNumber);
}
