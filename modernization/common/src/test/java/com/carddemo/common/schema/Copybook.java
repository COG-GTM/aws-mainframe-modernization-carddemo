package com.carddemo.common.schema;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/** Elementary items (level + name + PIC) of a copybook under {@code app/cpy}, in record order. */
public final class Copybook {

    private static final Pattern ITEM =
            Pattern.compile("^\\s*(\\d{2})\\s+([A-Z0-9-]+)\\s+PIC\\s+([^\\s.]+(?:\\.\\d+)?)");

    public record Item(String name, String picture) {
    }

    private Copybook() {
    }

    public static List<Item> elementaryItems(String name) {
        List<String> lines;
        try {
            lines = Files.readAllLines(MigrationDocs.copybook(name), StandardCharsets.ISO_8859_1);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
        List<Item> items = new ArrayList<>();
        for (String raw : lines) {
            if (raw.length() < 7 || raw.charAt(6) == '*') {
                continue;
            }
            String area = raw.substring(6, Math.min(raw.length(), 72));
            Matcher m = ITEM.matcher(area);
            if (m.find()) {
                items.add(new Item(m.group(2), m.group(3)));
            }
        }
        return items;
    }

    /** {@code 9(04)} and {@code 9(4)} denote the same picture. */
    public static String normalize(String picture) {
        return picture.replaceAll("\\(0*(\\d+)\\)", "($1)");
    }
}
