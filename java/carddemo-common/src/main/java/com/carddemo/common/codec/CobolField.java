package com.carddemo.common.codec;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/** Binds a Java record component to a copybook data name, e.g. {@code @CobolField("ACCT-CURR-BAL")}. */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.RECORD_COMPONENT)
public @interface CobolField {

    /** Data name in the copybook (searched within the enclosing group for nested records). */
    String value();

    /** Strip trailing spaces from alphanumeric values on decode (they are restored by padding on encode). */
    boolean trim() default true;
}
