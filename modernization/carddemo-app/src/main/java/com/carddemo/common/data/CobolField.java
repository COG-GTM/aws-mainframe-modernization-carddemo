package com.carddemo.common.data;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * The copybook elementary item a record component holds, e.g. {@code @CobolField("ACCT-ID")}. The name is the leaf
 * key from {@code RecordLayout.leaves()} and the first column of {@code db/copybook-column-map.csv}.
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.RECORD_COMPONENT, ElementType.FIELD, ElementType.PARAMETER, ElementType.METHOD})
public @interface CobolField {

    String value();
}
