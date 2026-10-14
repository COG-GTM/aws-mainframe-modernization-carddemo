package com.carddemo.runtime.batch;

import java.nio.file.Path;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;

/** DD name -> file path mapping for a step (the JCL DD statements, made configurable). */
public final class DdBindings {

    private final Map<String, Path> paths = new LinkedHashMap<>();

    public DdBindings bind(String ddName, Path path) {
        paths.put(ddName.trim().toUpperCase(), path);
        return this;
    }

    public Optional<Path> find(String ddName) {
        return Optional.ofNullable(paths.get(ddName.trim().toUpperCase()));
    }

    /** Resolves a DD; a missing DD is the JCL error IEC130I (file status 35 when the program opens it). */
    public Path path(String ddName) {
        return find(ddName).orElseThrow(() -> new IllegalStateException("DD statement missing: " + ddName));
    }

    public Map<String, Path> asMap() {
        return Map.copyOf(paths);
    }
}
