package com.carddemo.common;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.bind.DefaultValue;

@ConfigurationProperties(prefix = "carddemo")
public record CardDemoProperties(
        @DefaultValue Jwt jwt,
        @DefaultValue Aws aws,
        @DefaultValue Reports reports,
        @DefaultValue Messaging messaging,
        @DefaultValue Modules modules,
        @DefaultValue Seed seed) {

    public record Jwt(@DefaultValue("") String secret, @DefaultValue("60") long ttlMinutes) {
    }

    public record Aws(@DefaultValue("us-east-1") String region, @DefaultValue("carddemo-") String sqsQueuePrefix) {

        public String queueName(String logicalName) {
            return sqsQueuePrefix + logicalName;
        }
    }

    public record Reports(
            @DefaultValue("noop") String publisher,
            @DefaultValue("local") String status,
            @DefaultValue("") String stateMachineArn,
            @DefaultValue("") String s3Bucket) {
    }

    public record Messaging(@DefaultValue("false") boolean enabled) {
    }

    public record Modules(@DefaultValue("false") boolean authorizationsInstalled) {
    }

    public record Seed(@DefaultValue("") String asciiDir) {
    }
}
