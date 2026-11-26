package com.carddemo.programs.cbtrn01c.io;

import com.carddemo.programs.cbtrn01c.copybook.Pic;
import com.carddemo.programs.cbtrn01c.copybook.RecordCodec;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * KSDS emulation over a line-per-record fixed-width file: the whole file is loaded on OPEN and
 * indexed by the record key, which is the first {@code keyLength} bytes of the record image.
 */
public final class FixedWidthIndexedFile<R> implements IndexedInputFile<R> {

    private final Path path;
    private final RecordCodec<R> codec;
    private final int keyLength;
    private Map<String, R> index;

    public FixedWidthIndexedFile(Path path, RecordCodec<R> codec, int keyLength) {
        this.path = path;
        this.codec = codec;
        this.keyLength = keyLength;
    }

    @Override
    public String open() {
        if (!Files.isRegularFile(path)) {
            return FileStatus.FILE_NOT_FOUND;
        }
        List<String> lines;
        try {
            lines = Files.readAllLines(path, StandardCharsets.ISO_8859_1);
        } catch (IOException e) {
            return FileStatus.PERMANENT_ERROR;
        }
        Map<String, R> loaded = new HashMap<>();
        for (String line : lines) {
            if (line.length() > codec.length()) {
                return FileStatus.WRONG_LENGTH;
            }
            String image = Pic.alpha(line, codec.length());
            loaded.putIfAbsent(image.substring(0, keyLength), codec.parse(image));
        }
        index = loaded;
        return FileStatus.OK;
    }

    @Override
    public ReadResult<R> read(String key) {
        if (index == null) {
            return ReadResult.of(FileStatus.READ_NOT_OPEN);
        }
        R record = index.get(Pic.alpha(key, keyLength));
        return record == null ? ReadResult.of(FileStatus.RECORD_NOT_FOUND) : new ReadResult<>(FileStatus.OK, record);
    }

    @Override
    public String close() {
        if (index == null) {
            return FileStatus.CLOSE_NOT_OPEN;
        }
        index = null;
        return FileStatus.OK;
    }
}
