package com.carddemo.data.record;

import com.carddemo.common.codec.CobolField;

/** Fixed-width CARD-RECORD. Layout: copybook CVACT02Y. */
public record CardRecord(
        @CobolField("CARD-NUM") String cardNumber,
        @CobolField("CARD-ACCT-ID") Long accountId,
        @CobolField("CARD-CVV-CD") Integer cvvCode,
        @CobolField("CARD-EMBOSSED-NAME") String embossedName,
        @CobolField("CARD-EXPIRAION-DATE") String expirationDate,
        @CobolField("CARD-ACTIVE-STATUS") String activeStatus) {
}
