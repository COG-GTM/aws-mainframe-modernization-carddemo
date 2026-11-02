package com.carddemo.transaction.api.dto;

import com.carddemo.transaction.domain.TransactionCategoryType;

/** TRAN-CAT-RECORD (CVTRA04Y): TRAN-TYPE-CD, TRAN-CAT-CD, TRAN-CAT-TYPE-DESC. */
public record TransactionCategoryResponse(String typeCode, Integer categoryCode, String description) {

    public static TransactionCategoryResponse from(TransactionCategoryType category) {
        return new TransactionCategoryResponse(category.getId().getTypeCd(), category.getId().getCatCd(),
                category.getDescription());
    }
}
