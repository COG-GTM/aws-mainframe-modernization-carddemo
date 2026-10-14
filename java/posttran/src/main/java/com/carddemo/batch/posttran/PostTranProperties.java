package com.carddemo.batch.posttran;

import com.carddemo.batch.posttran.cobol.RecordFormat;
import com.carddemo.batch.posttran.seed.VsamDatasets;
import java.nio.file.Path;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.bind.DefaultValue;

/**
 * DD assignments for the POSTTRAN job.
 *
 * @param dataDir      root of the shipped seed data (app/data)
 * @param seed         dataset used to (re)load ACCOUNT/CARD_XREF/TRAN_CAT_BALANCE at startup, or null to skip
 * @param dalytran     DALYTRAN input; defaults to the seed's daily transaction file
 * @param dalyrejs     DALYREJS output
 * @param format       physical format of DALYTRAN/DALYREJS; defaults to the seed's format
 * @param exportDir    if set, TRANSACT/ACCOUNT/TCATBAL are unloaded here after the run
 * @param chunkSize    commit interval
 * @param runOnStartup run the job from the command line runner
 */
@ConfigurationProperties("posttran")
public record PostTranProperties(
        @DefaultValue("../../app/data") Path dataDir,
        @DefaultValue("ASCII") VsamDatasets.Seed seed,
        Path dalytran,
        @DefaultValue("target/dalyrejs.txt") Path dalyrejs,
        RecordFormat format,
        Path exportDir,
        @DefaultValue("1") int chunkSize,
        @DefaultValue("true") boolean runOnStartup) {
}
