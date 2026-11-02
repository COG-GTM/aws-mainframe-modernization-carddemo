package com.carddemo.transaction.api.dto;

import com.carddemo.transaction.domain.TransactionType;

/** TRAN-TYPE-RECORD (CVTRA03Y): TRAN-TYPE, TRAN-TYPE-DESC. */
public record TransactionTypeResponse(String typeCode, String description) {

    public static TransactionTypeResponse from(TransactionType type) {
        return new TransactionTypeResponse(type.getTypeCd(), type.getTypeDesc());
    }
}
