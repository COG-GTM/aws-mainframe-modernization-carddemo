package com.carddemo.batch.storage;

import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.List;
import java.util.stream.Stream;

/** Filesystem stand-in for the S3 bucket (local runner and tests): key = relative path. */
public class LocalObjectStore implements ObjectStore {

    private final Path root;

    public LocalObjectStore(Path root) {
        this.root = root.toAbsolutePath().normalize();
    }

    @Override
    public byte[] get(String key) {
        Path p = resolve(key);
        if (!Files.isRegularFile(p)) {
            throw new ObjectNotFoundException(uri(key));
        }
        try {
            return Files.readAllBytes(p);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    @Override
    public InputStream open(String key) {
        Path p = resolve(key);
        if (!Files.isRegularFile(p)) {
            throw new ObjectNotFoundException(uri(key));
        }
        try {
            return Files.newInputStream(p);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    @Override
    public boolean exists(String key) {
        return Files.isRegularFile(resolve(key));
    }

    @Override
    public void put(String key, byte[] content, String contentType) {
        Path p = resolve(key);
        try {
            Files.createDirectories(p.getParent());
            Files.write(p, content);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    @Override
    public void put(String key, Path file, String contentType) {
        Path p = resolve(key);
        try {
            Files.createDirectories(p.getParent());
            Files.copy(file, p, StandardCopyOption.REPLACE_EXISTING);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }


    @Override
    public List<String> list(String prefix) {
        if (!Files.isDirectory(root)) {
            return List.of();
        }
        try (Stream<Path> files = Files.walk(root)) {
            return files.filter(Files::isRegularFile)
                    .map(p -> root.relativize(p).toString().replace('\\', '/'))
                    .filter(k -> k.startsWith(prefix))
                    .sorted()
                    .toList();
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    @Override
    public String uri(String key) {
        return resolve(key).toUri().toString();
    }

    /** Rejects keys that leave the root lexically or through a symbolic link. */
    private Path resolve(String key) {
        Path p = root.resolve(key).normalize();
        if (!p.startsWith(root)) {
            throw new IllegalArgumentException("Key escapes storage root: " + key);
        }
        Path existing = p;
        while (existing != null && !Files.exists(existing)) {
            existing = existing.getParent();
        }
        try {
            if (existing != null && Files.exists(root) && !existing.toRealPath().startsWith(root.toRealPath())) {
                throw new IllegalArgumentException("Key escapes storage root: " + key);
            }
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
        return p;
    }
}
