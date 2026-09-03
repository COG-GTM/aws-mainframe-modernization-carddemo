package com.carddemo.posting.domain;

import java.math.BigDecimal;

/**
 * An unposted transaction read from the daily transaction file
 * ({@code CVTRA06Y}, {@code app/cpy/CVTRA06Y.cpy:4-19}), delivered through DD {@code DALYTRAN}
 * ({@code app/jcl/POSTTRAN.jcl:30-31}).
 *
 * <p>{@code DALYTRAN-PROC-TS} is blank on every record of the shipped file: the processing
 * timestamp is what the posting run stamps on, so it is not modelled as an input value beyond the
 * raw field.
 */
public record DailyTransaction(String id,
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

    /**
     * The origination date, i.e. the first ten characters of the origination timestamp, as
     * compared against the account expiry date at {@code app/cbl/CBTRN02C.cbl:414}.
     */
    public String originationDate() {
        return originTimestamp.substring(0, 10);
    }
}
