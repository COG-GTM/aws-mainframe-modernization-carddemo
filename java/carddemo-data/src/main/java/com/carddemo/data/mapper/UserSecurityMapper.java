package com.carddemo.data.mapper;

import com.carddemo.data.entity.UserSecurity;
import com.carddemo.data.record.UserSecurityRecord;

import org.mapstruct.Mapper;

/** UserSecurityRecord (copybook layout) <-> UserSecurity (JPA entity). */
@Mapper(uses = CobolFormats.class)
public interface UserSecurityMapper {

    UserSecurity toEntity(UserSecurityRecord record);

    UserSecurityRecord toRecord(UserSecurity entity);
}
