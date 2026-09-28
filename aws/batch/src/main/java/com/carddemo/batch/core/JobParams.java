package com.carddemo.batch.core;

import java.time.LocalDate;
import java.time.format.DateTimeParseException;
import java.util.Map;
import java.util.Optional;

/** {@code --job=<name> --runId=<id> --businessDate=yyyy-MM-dd [--<param>=<value> …]}. */
public record JobParams(String jobName, String runId, LocalDate businessDate, Map<String, String> params) {

    public Optional<String> get(String name) {
        return Optional.ofNullable(params.get(name)).filter(v -> !v.isBlank());
    }

    public String require(String name) {
        return get(name).orElseThrow(
                () -> new JobFailure(ReturnCode.INPUT_ERROR, "Missing required parameter --" + name));
    }

    public LocalDate date(String name) {
        String v = require(name);
        try {
            return LocalDate.parse(v);
        } catch (DateTimeParseException e) {
            throw new JobFailure(ReturnCode.INPUT_ERROR, "--" + name + " must be yyyy-MM-dd: " + v);
        }
    }
}
