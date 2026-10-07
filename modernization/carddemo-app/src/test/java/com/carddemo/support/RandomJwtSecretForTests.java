package com.carddemo.support;

import java.security.SecureRandom;
import java.util.Base64;
import java.util.Map;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.env.EnvironmentPostProcessor;
import org.springframework.core.env.ConfigurableEnvironment;
import org.springframework.core.env.MapPropertySource;

/**
 * Test classpath only: when the environment has no {@code CARDDEMO_JWT_SECRET}, every test context of this JVM gets the
 * same random 48-byte key, so no key is committed anywhere (s6.4). Lowest precedence: explicit properties win.
 */
public class RandomJwtSecretForTests implements EnvironmentPostProcessor {

    /** Resolved by {@code ${CARDDEMO_JWT_SECRET:}} in application.yml, like the real environment variable. */
    private static final String KEY = "CARDDEMO_JWT_SECRET";
    private static final String SECRET = random();

    private static String random() {
        byte[] bytes = new byte[48];
        new SecureRandom().nextBytes(bytes);
        return Base64.getEncoder().encodeToString(bytes);
    }

    @Override
    public void postProcessEnvironment(ConfigurableEnvironment environment, SpringApplication application) {
        environment.getPropertySources().addLast(new MapPropertySource("randomJwtSecretForTests",
                Map.of(KEY, SECRET)));
    }
}
