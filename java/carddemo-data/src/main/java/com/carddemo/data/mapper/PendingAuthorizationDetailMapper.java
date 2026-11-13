package com.carddemo.data.mapper;

import com.carddemo.data.entity.PendingAuthorizationDetail;
import com.carddemo.data.record.PendingAuthDetailRecord;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

/** PAUTDTL1 segment <-> entity; the parent PAUTSUM0 account id completes the key. */
@Mapper(uses = CobolFormats.class)
public interface PendingAuthorizationDetailMapper {

    @Mapping(target = "id.accountId", source = "accountId")
    @Mapping(target = "id.authDateKey", source = "record.key.authDate")
    @Mapping(target = "id.authTimeKey", source = "record.key.authTime")
    PendingAuthorizationDetail toEntity(PendingAuthDetailRecord record, Long accountId);

    @Mapping(target = "key.authDate", source = "id.authDateKey")
    @Mapping(target = "key.authTime", source = "id.authTimeKey")
    PendingAuthDetailRecord toRecord(PendingAuthorizationDetail entity);
}
