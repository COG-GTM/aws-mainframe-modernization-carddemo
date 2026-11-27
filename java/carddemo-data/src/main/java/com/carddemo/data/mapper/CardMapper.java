package com.carddemo.data.mapper;

import com.carddemo.data.entity.Card;
import com.carddemo.data.record.CardRecord;

import org.mapstruct.Mapper;

/** CardRecord (copybook layout) <-> Card (JPA entity). */
@Mapper(uses = CobolFormats.class)
public interface CardMapper {

    Card toEntity(CardRecord record);

    CardRecord toRecord(Card entity);
}
