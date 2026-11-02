package com.carddemo.common.api;

import com.fasterxml.jackson.annotation.JacksonAnnotationsInside;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Marks a numeric id that is {@code PIC 9(digits)} in the COBOL record. It is stored as a number
 * (BIGINT) and written to JSON as a string with its leading zeros, e.g. account id 1 as
 * {@code "00000000001"}.
 */
@Target({ElementType.FIELD, ElementType.METHOD, ElementType.RECORD_COMPONENT, ElementType.PARAMETER})
@Retention(RetentionPolicy.RUNTIME)
@JacksonAnnotationsInside
@JsonSerialize(using = PicIdSerializer.class)
public @interface PicId {

    int digits();
}
