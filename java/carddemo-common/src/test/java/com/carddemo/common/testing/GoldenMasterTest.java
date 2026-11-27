package com.carddemo.common.testing;

import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.extension.ExtendWith;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Marks a golden-master (mainframe parity) test class: tagged {@code golden-master} so it can be run on its own
 * ({@code mvn verify -Dgroups=golden-master}), with {@link MainframeSources} and {@link GoldenMaster} injectable.
 */
@Target({ElementType.TYPE, ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
@Tag("golden-master")
@ExtendWith(GoldenMasterExtension.class)
public @interface GoldenMasterTest {
}
