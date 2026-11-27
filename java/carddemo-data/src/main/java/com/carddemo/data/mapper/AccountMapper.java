package com.carddemo.data.mapper;

import com.carddemo.data.entity.Account;
import com.carddemo.data.record.AccountRecord;

import org.mapstruct.Mapper;

/** AccountRecord (copybook layout) <-> Account (JPA entity). */
@Mapper(uses = CobolFormats.class)
public interface AccountMapper {

    Account toEntity(AccountRecord record);

    AccountRecord toRecord(Account entity);
}
