package com.carddemo.interestcalc;

import java.time.Clock;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;

/**
 * Spring Boot entry point for the migrated CBACT04C batch job.
 *
 * <p>Equivalent of {@code //STEP15 EXEC PGM=CBACT04C,PARM='2022071800'}:
 *
 * <pre>
 * java -jar interest-calculator.jar \
 *      runDate=2022071800 \
 *      inputDirectory=app/data/ASCII \
 *      outputDirectory=target/run
 * </pre>
 */
@SpringBootApplication
public class InterestCalculatorApplication {

    public static void main(String[] args) {
        SpringApplication.run(InterestCalculatorApplication.class, args);
    }

    /**
     * The clock behind {@code FUNCTION CURRENT-DATE}. It is a bean so that the parity harness
     * can pin {@code TRAN-ORIG-TS} and {@code TRAN-PROC-TS}, the job's only nondeterministic
     * outputs.
     */
    @Bean
    public Clock clock() {
        return Clock.systemDefaultZone();
    }
}
