package com.carddemo.common.schema;

import static org.assertj.core.api.Assertions.assertThat;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import javax.sql.DataSource;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * Checks a service schema, as created by its Flyway migrations, against
 * {@code docs/migration/schema-mapping.md}: every documented column exists with the documented type
 * and every column of the schema is documented. Subclasses are Spring Boot tests of one service.
 */
public abstract class AbstractSchemaMappingTest {

    @Autowired
    private DataSource dataSource;

    private final String schema;

    protected AbstractSchemaMappingTest(String schema) {
        this.schema = schema;
    }

    @Test
    void everyDocumentedColumnExistsWithItsType() throws SQLException {
        Map<String, String> actual = columnTypes();
        List<SchemaMapping.Section> sections = SchemaMapping.load().forSchema(schema);
        assertThat(sections).as("schema-mapping.md sections for schema %s", schema).isNotEmpty();
        Map<String, String> documented = new LinkedHashMap<>();
        for (SchemaMapping.Section section : sections) {
            for (SchemaMapping.Row row : section.columns()) {
                documented.put(section.table() + "." + row.column(), row.type().replace(" ", ""));
            }
        }
        assertThat(actual).containsAllEntriesOf(documented);
    }

    @Test
    void everySchemaColumnIsDocumented() throws SQLException {
        List<String> documented = new ArrayList<>();
        for (SchemaMapping.Section section : SchemaMapping.load().forSchema(schema)) {
            section.columns().forEach(row -> documented.add(section.table() + "." + row.column()));
        }
        assertThat(columnTypes().keySet()).containsExactlyInAnyOrderElementsOf(documented);
    }

    /** {@code table.column -> TYPE} for the schema's application tables (Flyway and Spring Batch tables excluded). */
    private Map<String, String> columnTypes() throws SQLException {
        Map<String, String> types = new LinkedHashMap<>();
        String sql = """
                SELECT table_name, column_name, data_type, character_maximum_length, numeric_precision, numeric_scale
                FROM information_schema.columns
                WHERE table_schema = ? AND table_name <> 'flyway_schema_history' AND table_name NOT LIKE 'batch\\_%'
                ORDER BY table_name, ordinal_position
                """;
        try (Connection connection = dataSource.getConnection();
             var statement = connection.prepareStatement(sql)) {
            statement.setString(1, schema);
            try (ResultSet rs = statement.executeQuery()) {
                while (rs.next()) {
                    types.put(rs.getString(1) + "." + rs.getString(2), sqlType(rs));
                }
            }
        }
        return types;
    }

    private static String sqlType(ResultSet rs) throws SQLException {
        String dataType = rs.getString(3);
        return switch (dataType) {
            case "character varying" -> "VARCHAR(" + rs.getInt(4) + ")";
            case "numeric" -> "NUMERIC(" + rs.getInt(5) + "," + rs.getInt(6) + ")";
            case "timestamp without time zone" -> "TIMESTAMP";
            default -> dataType.toUpperCase();
        };
    }
}
