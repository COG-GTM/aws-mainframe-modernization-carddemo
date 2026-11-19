package com.carddemo.reference.batch;

import com.carddemo.common.cobol.CodePage;
import com.carddemo.common.record.CobolRecord;
import com.carddemo.common.record.RecordLayout;
import com.carddemo.reference.domain.TranCatBalance;
import com.carddemo.reference.domain.TranCatBalanceId;

/** CVTRA01Y TRAN-CAT-BAL-RECORD (TCATBALF, 50 bytes) and its mapping to {@link TranCatBalance}. */
public final class TranCatBalanceRecordLayout {

    /**
     * The 22-byte FILLER is not stored. Every TCATBALF record in the sample data and in the GnuCOBOL oracle output
     * (including records CBTRN02C creates) carries zeros there, so a record image is rebuilt with zeros.
     */
    public static final RecordLayout LAYOUT = RecordLayout.builder("CVTRA01Y", 50)
            .zoned("TRANCAT-ACCT-ID", 11, 0, false)
            .alphanumeric("TRANCAT-TYPE-CD", 2)
            .zoned("TRANCAT-CD", 4, 0, false)
            .zoned("TRAN-CAT-BAL", 9, 2, true)
            .alphanumeric("TRAN-CAT-FILLER", 22)
            .build();

    static final String FILLER = "0".repeat(22);

    private TranCatBalanceRecordLayout() {
    }

    public static TranCatBalance toEntity(CobolRecord r) {
        return new TranCatBalance(new TranCatBalanceId(r.getLong("TRANCAT-ACCT-ID"), r.getString("TRANCAT-TYPE-CD"),
                r.getLong("TRANCAT-CD")), r.getDecimal("TRAN-CAT-BAL"));
    }

    public static CobolRecord toRecord(TranCatBalance t, CodePage codePage) {
        TranCatBalanceId id = t.getId();
        return LAYOUT.blank(codePage)
                .setLong("TRANCAT-ACCT-ID", id.acctId())
                .setString("TRANCAT-TYPE-CD", id.typeCd())
                .setLong("TRANCAT-CD", id.catCd())
                .setDecimal("TRAN-CAT-BAL", t.getBalance())
                .setString("TRAN-CAT-FILLER", FILLER);
    }
}
