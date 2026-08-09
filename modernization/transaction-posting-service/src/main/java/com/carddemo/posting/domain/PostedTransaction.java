package com.carddemo.posting.domain;

import java.math.BigDecimal;

/**
 * A transaction written to the transaction master ({@code CVTRA05Y},
 * {@code app/cpy/CVTRA05Y.cpy:4-18}), delivered through DD {@code TRANFILE}
 * ({@code app/jcl/POSTTRAN.jcl:28-29}).
 *
 * <p>Every field except the processing timestamp is copied verbatim from the daily transaction
 * ({@code app/cbl/CBTRN02C.cbl:425-438}), so posting keeps the originating transaction id rather
 * than allocating a new one.
 */
public record PostedTransaction(String id,
                                String typeCode,
                                String categoryCode,
                                String source,
                                String description,
                                BigDecimal amount,
                                String merchantId,
                                String merchantName,
                                String merchantCity,
                                String merchantZip,
                                CardNumber cardNumber,
                                String originTimestamp,
                                String processTimestamp) {
}
