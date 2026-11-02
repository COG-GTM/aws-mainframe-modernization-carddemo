package com.carddemo.batch.tranrept;

import com.carddemo.common.record.CobolRecord;
import com.carddemo.common.record.layout.CardXrefRecordLayout;
import com.carddemo.common.record.layout.TranCatRecordLayout;
import com.carddemo.common.record.layout.TranTypeRecordLayout;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/** CARDXREF, TRANTYPE and TRANCATG as DD files (KSDS unloads), keyed on the record key bytes. */
public final class FileReferenceData implements ReferenceData {

    private final Map<String, Long> xref = new HashMap<>();
    private final Map<String, String> types = new HashMap<>();
    private final Map<String, String> categories = new HashMap<>();

    public FileReferenceData(List<CobolRecord> cardXref, List<CobolRecord> tranType, List<CobolRecord> tranCatg) {
        cardXref.forEach(r -> xref.putIfAbsent(r.get(CardXrefRecordLayout.XREF_CARD_NUM),
                r.getLong(CardXrefRecordLayout.XREF_ACCT_ID)));
        tranType.forEach(r -> types.putIfAbsent(r.get(TranTypeRecordLayout.TRAN_TYPE),
                r.get(TranTypeRecordLayout.TRAN_TYPE_DESC)));
        tranCatg.forEach(r -> categories.putIfAbsent(r.get(TranCatRecordLayout.TRAN_CAT_KEY),
                r.get(TranCatRecordLayout.TRAN_CAT_TYPE_DESC)));
    }

    @Override
    public Optional<Long> accountIdForCard(String cardNumber) {
        return Optional.ofNullable(xref.get(cardNumber));
    }

    @Override
    public Optional<String> typeDescription(String typeCode) {
        return Optional.ofNullable(types.get(typeCode));
    }

    @Override
    public Optional<String> categoryDescription(String typeCode, int categoryCode) {
        return Optional.ofNullable(categories.get(typeCode + String.format("%04d", categoryCode)));
    }
}
