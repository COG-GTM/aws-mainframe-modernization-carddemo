package com.carddemo.common.security;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Restricts a controller (or one handler method) to {@code CDEMO-USRTYP-ADMIN} callers; a {@code U}
 * token gets 403. Mirrors the COADM01C menu, whose options are only reachable by admins.
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
public @interface AdminOnly {
}
