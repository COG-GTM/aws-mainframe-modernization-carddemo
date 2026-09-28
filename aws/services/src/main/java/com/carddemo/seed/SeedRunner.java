package com.carddemo.seed;

import com.carddemo.common.CardDemoProperties;
import java.nio.file.Files;
import java.nio.file.Path;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.stereotype.Component;

/** Seeds an empty database from {@code carddemo.seed.ascii-dir} when that property is set. */
@Component
public class SeedRunner implements ApplicationRunner {

    private static final Logger LOG = LoggerFactory.getLogger(SeedRunner.class);

    private final CardDemoProperties properties;
    private final AsciiFixtureLoader loader;

    public SeedRunner(CardDemoProperties properties, AsciiFixtureLoader loader) {
        this.properties = properties;
        this.loader = loader;
    }

    @Override
    public void run(ApplicationArguments args) {
        String dir = properties.seed().asciiDir();
        if (dir == null || dir.isBlank()) {
            return;
        }
        Path path = Path.of(dir);
        if (!Files.isDirectory(path)) {
            LOG.warn("Seed directory {} does not exist; skipping fixture load", path.toAbsolutePath());
            return;
        }
        if (!loader.isEmpty()) {
            LOG.info("Database already contains data; skipping fixture load");
            return;
        }
        loader.load(path);
    }
}
