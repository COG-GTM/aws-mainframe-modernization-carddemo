package com.carddemo.common.schema;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/** Parsed {@code docs/migration/schema-mapping.md}: one section per record layout and target table. */
public record SchemaMapping(List<Section> sections) {

    private static final Pattern TABLE = Pattern.compile("`(\\w+)\\.(\\w+)`");
    private static final Pattern TICKED = Pattern.compile("`([A-Z0-9-]+)`");

    /** A documented row. {@code field} is null for a column with no copybook field; {@code column} is null when dropped. */
    public record Row(String field, String picture, String column, String type) {
        public boolean dropped() {
            return column == null;
        }
    }

    /** {@code copybook} is null when the record is defined inline in a program (DALYREJS). */
    public record Section(String heading, String copybook, String schema, String table, List<Row> rows) {
        public List<Row> columns() {
            return rows.stream().filter(r -> !r.dropped()).toList();
        }

        public List<Row> copybookFields() {
            return rows.stream().filter(r -> r.field() != null).toList();
        }
    }

    public static SchemaMapping load() {
        return parse(MigrationDocs.schemaMapping());
    }

    public static SchemaMapping parse(Path file) {
        List<String> lines;
        try {
            lines = Files.readAllLines(file);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
        List<Section> sections = new ArrayList<>();
        String heading = null;
        String copybook = null;
        String schema = null;
        String table = null;
        List<Row> rows = new ArrayList<>();
        for (String line : lines) {
            if (line.startsWith("## ")) {
                if (table != null) {
                    sections.add(new Section(heading, copybook, schema, table, List.copyOf(rows)));
                }
                heading = line.substring(3).trim();
                rows.clear();
                Matcher target = TABLE.matcher(heading);
                if (target.find()) {
                    schema = target.group(1);
                    table = target.group(2);
                } else {
                    schema = null;
                    table = null;
                }
                copybook = null;
                Matcher ticked = TICKED.matcher(heading);
                while (ticked.find()) {
                    if (Files.exists(MigrationDocs.copybook(ticked.group(1)))) {
                        copybook = ticked.group(1);
                        break;
                    }
                }
            } else if (table != null && line.startsWith("|") && !line.startsWith("| Field") && !line.startsWith("|---")) {
                String[] cells = Arrays.stream(line.split("\\|", -1)).map(String::trim).toArray(String[]::new);
                // cells[0] is empty (text before the first pipe)
                String field = cells[1].startsWith("*(new)*") ? null : unticked(cells[1]);
                String column = cells[3].equals("—") ? null : unticked(cells[3]);
                rows.add(new Row(field, field == null ? null : unticked(cells[2]), column, unticked(cells[4])));
            }
        }
        if (table != null) {
            sections.add(new Section(heading, copybook, schema, table, List.copyOf(rows)));
        }
        return new SchemaMapping(List.copyOf(sections));
    }

    public List<Section> forSchema(String schema) {
        return sections.stream().filter(s -> s.schema().equals(schema)).toList();
    }

    private static String unticked(String cell) {
        return cell.replace("`", "").trim();
    }
}
