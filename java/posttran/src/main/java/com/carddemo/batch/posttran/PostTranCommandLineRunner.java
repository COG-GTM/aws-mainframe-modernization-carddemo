package com.carddemo.batch.posttran;

import com.carddemo.batch.posttran.batch.PostTranLauncher;
import com.carddemo.batch.posttran.batch.PostTranResult;
import com.carddemo.batch.posttran.cobol.RecordFormat;
import com.carddemo.batch.posttran.seed.VsamDatasets;
import java.nio.file.Path;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.boot.ExitCodeGenerator;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Component;

/** Seeds the tables (optional), runs POSTTRAN, unloads the tables (optional) and sets the exit code. */
@Component
@ConditionalOnProperty(name = "posttran.run-on-startup", havingValue = "true", matchIfMissing = true)
public class PostTranCommandLineRunner implements ApplicationRunner, ExitCodeGenerator {

    private final PostTranProperties properties;
    private final VsamDatasets datasets;
    private final PostTranLauncher launcher;
    private int exitCode;

    public PostTranCommandLineRunner(PostTranProperties properties, VsamDatasets datasets, PostTranLauncher launcher) {
        this.properties = properties;
        this.datasets = datasets;
        this.launcher = launcher;
    }

    @Override
    public void run(ApplicationArguments args) {
        VsamDatasets.Seed seed = properties.seed();
        if (seed != null) {
            datasets.load(properties.dataDir(), seed);
        }
        RecordFormat format = properties.format() != null ? properties.format()
                : seed != null ? seed.format : RecordFormat.ASCII;
        Path dalytran = properties.dalytran() != null ? properties.dalytran()
                : (seed != null ? seed : VsamDatasets.Seed.ASCII).dalytran(properties.dataDir());
        PostTranResult result = launcher.run(dalytran, properties.dalyrejs(), format);
        if (properties.exportDir() != null && !result.abended()) {
            datasets.unloadAll(properties.exportDir(), format);
        }
        exitCode = result.returnCode();
    }

    @Override
    public int getExitCode() {
        return exitCode;
    }
}
