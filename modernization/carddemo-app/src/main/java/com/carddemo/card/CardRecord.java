package com.carddemo.card;

import com.carddemo.common.data.CobolField;
import com.carddemo.common.data.CopybookRecordMapper;

/**
 * One CARDDATA record (copybook CVACT02Y) as an immutable value. {@link #MAPPER} converts it to and from
 * the fixed-width record; {@link Card} persists it.
 *
 * @param cardNum CARD-NUM PIC X(16)
 * @param acctId CARD-ACCT-ID PIC 9(11)
 * @param cvvCd CARD-CVV-CD PIC 9(03)
 * @param embossedName CARD-EMBOSSED-NAME PIC X(50)
 * @param expirationDate CARD-EXPIRAION-DATE PIC X(10)
 * @param activeStatus CARD-ACTIVE-STATUS PIC X(01)
 */
public record CardRecord(
        @CobolField("CARD-NUM") String cardNum,
        @CobolField("CARD-ACCT-ID") long acctId,
        @CobolField("CARD-CVV-CD") int cvvCd,
        @CobolField("CARD-EMBOSSED-NAME") String embossedName,
        @CobolField("CARD-EXPIRAION-DATE") String expirationDate,
        @CobolField("CARD-ACTIVE-STATUS") CardStatus activeStatus) {

    public static final CopybookRecordMapper<CardRecord> MAPPER =
            CopybookRecordMapper.of(CardRecord.class, "CVACT02Y");
}
