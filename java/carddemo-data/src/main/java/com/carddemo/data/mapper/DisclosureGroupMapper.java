package com.carddemo.data.mapper;

import com.carddemo.data.entity.DisclosureGroup;
import com.carddemo.data.record.DisclosureGroupRecord;

import org.mapstruct.Mapper;

/** DisclosureGroupRecord (copybook layout) <-> DisclosureGroup (JPA entity). */
@Mapper(uses = CobolFormats.class)
public interface DisclosureGroupMapper {

    DisclosureGroup toEntity(DisclosureGroupRecord record);

    DisclosureGroupRecord toRecord(DisclosureGroup entity);
}
