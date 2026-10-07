package com.carddemo.batch;

import java.time.LocalDate;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * JCL parameters pinned for golden-set runs against the GnuCOBOL baseline (ADR-0014). All values are empty
 * unless the {@code golden} profile (or an operator) sets them; jobs then fall back to the business clock.
 *
 * @param intcalcParmDate      {@code PARM='2022071800'} of {@code INTCALC.jcl} step {@code STEP15} (CBACT04C)
 * @param tranreptStartDate    {@code PARM-START-DATE} of {@code TRANREPT.jcl} SYMNAMES / DATEPARM
 * @param tranreptEndDate      {@code PARM-END-DATE} of {@code TRANREPT.jcl} SYMNAMES / DATEPARM
 * @param waitstepCentiseconds {@code SYSIN 00003600} of {@code WAITSTEP.jcl} (COBSWAIT / MVSWAIT)
 */
@ConfigurationProperties("carddemo.baseline")
public record BaselineRunProperties(
        String intcalcParmDate,
        LocalDate tranreptStartDate,
        LocalDate tranreptEndDate,
        Integer waitstepCentiseconds) {
}
