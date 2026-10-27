package com.carddemo.transaction;

import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * External file locations that replace the JCL DD statements of POSTTRAN / the CBTRN01C job.
 */
@ConfigurationProperties(prefix = "carddemo")
public record CardDemoProperties(Batch batch, Seed seed) {

    /**
     * @param dalytranFile  DD DALYTRAN - daily transaction sequential input
     * @param dalyrejsDir   DD DALYREJS - directory receiving a new reject "generation" per run (GDG +1)
     * @param chunkSize     commit interval
     */
    public record Batch(String dalytranFile, String dalyrejsDir, int chunkSize) {
    }

    /**
     * Initial load of the VSAM KSDS clusters (IDCAMS REPRO equivalent) from the ASCII extracts in app/data/ASCII.
     */
    public record Seed(boolean enabled, String dataDir) {
    }
}
