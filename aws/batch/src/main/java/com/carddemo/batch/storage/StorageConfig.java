package com.carddemo.batch.storage;

import java.nio.file.Path;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import software.amazon.awssdk.services.s3.S3Client;

@Configuration
public class StorageConfig {

    /**
     * {@code S3_BUCKET} (conventions.md) selects S3; {@code carddemo.storage.local-dir} (local runner/tests only)
     * replaces the bucket with a directory. The S3 client takes its region from {@code AWS_REGION}.
     */
    @Bean
    public ObjectStore objectStore(@Value("${carddemo.storage.local-dir:}") String localDir,
            @Value("${S3_BUCKET:}") String bucket) {
        if (!localDir.isBlank()) {
            return new LocalObjectStore(Path.of(localDir));
        }
        if (bucket.isBlank()) {
            throw new IllegalStateException("S3_BUCKET is not set (or carddemo.storage.local-dir for local runs)");
        }
        return new S3ObjectStore(S3Client.create(), bucket);
    }
}
