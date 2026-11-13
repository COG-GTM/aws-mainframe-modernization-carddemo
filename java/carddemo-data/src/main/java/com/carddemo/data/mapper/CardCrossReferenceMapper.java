package com.carddemo.data.mapper;

import com.carddemo.data.entity.CardCrossReference;
import com.carddemo.data.record.CardXrefRecord;

import org.mapstruct.Mapper;

/** CardXrefRecord (copybook layout) <-> CardCrossReference (JPA entity). */
@Mapper(uses = CobolFormats.class)
public interface CardCrossReferenceMapper {

    CardCrossReference toEntity(CardXrefRecord record);

    CardXrefRecord toRecord(CardCrossReference entity);
}
