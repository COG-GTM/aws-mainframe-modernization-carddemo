package com.carddemo.batch.tranrept;

import java.util.Optional;

/** The three KSDS reads of CBTRN03C, keyed exactly as the COBOL keys them. */
public interface ReferenceData {

    /** {@code 1500-A-LOOKUP-XREF}: XREF-ACCT-ID of CARDXREF record {@code cardNumber} (16 bytes). */
    Optional<Long> accountIdForCard(String cardNumber);

    /** {@code 1500-B-LOOKUP-TRANTYPE}: TRAN-TYPE-DESC of TRANTYPE record {@code typeCode} (2 bytes). */
    Optional<String> typeDescription(String typeCode);

    /** {@code 1500-C-LOOKUP-TRANCATG}: TRAN-CAT-TYPE-DESC of TRANCATG record {@code typeCode + categoryCode}. */
    Optional<String> categoryDescription(String typeCode, int categoryCode);

    /** CARDXREF reads from {@code xref}, TRANTYPE from {@code types}, TRANCATG from {@code categories}. */
    static ReferenceData combine(ReferenceData xref, ReferenceData types, ReferenceData categories) {
        return new ReferenceData() {
            @Override
            public Optional<Long> accountIdForCard(String cardNumber) {
                return xref.accountIdForCard(cardNumber);
            }

            @Override
            public Optional<String> typeDescription(String typeCode) {
                return types.typeDescription(typeCode);
            }

            @Override
            public Optional<String> categoryDescription(String typeCode, int categoryCode) {
                return categories.categoryDescription(typeCode, categoryCode);
            }
        };
    }
}
