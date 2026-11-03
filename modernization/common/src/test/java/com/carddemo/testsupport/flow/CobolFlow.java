package com.carddemo.testsupport.flow;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Marks an online flow test with the CICS program, paragraph and screen key it covers. The test method
 * name must start with {@code <PROGRAM>_<PARAGRAPH>} (dashes as underscores) so the surefire reports,
 * which only carry method names, name the COBOL code too; {@link FlowCoverage} checks both.
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface CobolFlow {

    /** Program id, e.g. {@code COSGN00C}. */
    String program();

    /** Paragraph whose behavior the test asserts, e.g. {@code READ-USER-SEC-FILE}. */
    String paragraph();

    /** The 3270 key that starts the server round trip. */
    ScreenKey key();
}
