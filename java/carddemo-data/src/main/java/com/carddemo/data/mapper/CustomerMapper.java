package com.carddemo.data.mapper;

import com.carddemo.data.entity.Customer;
import com.carddemo.data.record.CustomerRecord;

import org.mapstruct.InheritInverseConfiguration;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

/** CustomerRecord (flat CUST-ADDR-* items) <-> Customer (embedded Address). */
@Mapper(uses = CobolFormats.class)
public interface CustomerMapper {

    @Mapping(target = "address.line1", source = "addressLine1")
    @Mapping(target = "address.line2", source = "addressLine2")
    @Mapping(target = "address.line3", source = "addressLine3")
    @Mapping(target = "address.stateCode", source = "addressStateCode")
    @Mapping(target = "address.countryCode", source = "addressCountryCode")
    @Mapping(target = "address.zip", source = "addressZip")
    Customer toEntity(CustomerRecord record);

    @InheritInverseConfiguration
    CustomerRecord toRecord(Customer entity);
}
