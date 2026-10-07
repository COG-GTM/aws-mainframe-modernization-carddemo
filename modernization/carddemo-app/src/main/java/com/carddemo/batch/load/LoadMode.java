package com.carddemo.batch.load;

/** How {@link VsamDatasetLoader} treats rows already in the target table. */
public enum LoadMode {

    /** Empty the table, then insert every record: IDCAMS {@code DELETE}/{@code DEFINE} + {@code REPRO}. */
    REPLACE,

    /**
     * Replace the rows whose key is in the file and keep the others: IDCAMS {@code REPRO ... REPLACE} into an
     * existing KSDS.
     */
    UPSERT
}
