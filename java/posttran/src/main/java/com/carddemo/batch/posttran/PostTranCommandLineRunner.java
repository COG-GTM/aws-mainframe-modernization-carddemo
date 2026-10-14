package com.carddemo.batch.posttran;

import com.carddemo.batch.posttran.batch.PostTranLauncher;
import com.carddemo.batch.posttran.batch.PostTranResult;
import com.carddemo.batch.posttran.cobol.RecordFormat;
import com.carddemo.batch.posttran.cobol.Sysout;
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

    /** Postings committed, but unloading TRANSACT/ACCTDATA/TCATBAL to posttran.export-dir failed. */
    public static final int EXPORT_FAILED = 8;

    private final PostTranProperties properties;
    private final VsamDatasets datasets;
    private final PostTranLauncher launcher;
    private final Sysout sysout;
    private int exitCode;

    public PostTranCommandLineRunner(PostTranProperties properties, VsamDatasets datasets, PostTranLauncher launcher,
            Sysout sysout) {
        this.properties = properties;
        this.datasets = datasets;
        this.launcher = launcher;
        this.sysout = sysout;
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
        exitCode = result.returnCode();
        if (properties.exportDir() != null && !result.abended()) {
            try {
                datasets.unloadAll(properties.exportDir(), format);
            } catch (RuntimeException e) {
                // POSTTRAN itself completed and committed; only the follow-on unload (an IDCAMS REPRO step) failed.
                sysout.display("EXPORT OF POSTED DATASETS FAILED: " + e.getMessage());
                exitCode = EXPORT_FAILED;
            }
        }
    }

    @Override
    public int getExitCode() {
        return exitCode;
    }
}
