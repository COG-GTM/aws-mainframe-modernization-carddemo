package com.carddemo.loader;

import com.carddemo.common.cobol.CobolValues;
import com.carddemo.common.record.AlphanumericField;
import com.carddemo.common.record.CobolRecord;
import com.carddemo.common.record.NumericField;

import java.math.BigDecimal;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Types;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;

/** Conversions between a record field and its column value (conventions.md §1, §4). */
final class FieldValues {

    /** The timestamp layout of the sample files ({@code dailytran.txt}). */
    static final DateTimeFormatter FILE_TIMESTAMP = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSSSSS");

    private FieldValues() {
    }

    /**
     * The value stored in the column. A non-blank date or timestamp that does not parse is an error:
     * loading it as null would silently lose data.
     */
    static Object toColumn(CobolRecord record, Column column) {
        return switch (column.kind()) {
            case TEXT -> CobolValues.trim(record.get((AlphanumericField) column.field()));
            case LONG -> record.getLong((NumericField) column.field());
            case INT -> record.getInt((NumericField) column.field());
            case DECIMAL -> record.get((NumericField) column.field());
            case DATE -> parse(record, column, true);
            case TIMESTAMP -> parse(record, column, false);
            case PASSWORD -> plainPassword(record, column);
        };
    }

    /** {@code SEC-USR-PWD} without its blank padding; only ever hashed. */
    static String plainPassword(CobolRecord record, Column column) {
        return record.get((AlphanumericField) column.field()).stripTrailing();
    }

    private static Object parse(CobolRecord record, Column column, boolean date) {
        String text = CobolValues.trim(record.get((AlphanumericField) column.field()));
        if (text == null) {
            return null;
        }
        try {
            return date ? LocalDate.parse(text) : LocalDateTime.parse(text, FILE_TIMESTAMP);
        } catch (DateTimeParseException ex) {
            throw new IllegalArgumentException(column.field().name() + " is not a valid "
                    + (date ? "date" : "timestamp") + ": '" + text + "'", ex);
        }
    }

    static void bind(PreparedStatement statement, int index, Column column, Object value) throws SQLException {
        if (value == null) {
            statement.setNull(index, switch (column.kind()) {
                case TEXT, PASSWORD -> Types.VARCHAR;
                case LONG -> Types.BIGINT;
                case INT -> Types.INTEGER;
                case DECIMAL -> Types.NUMERIC;
                case DATE -> Types.DATE;
                case TIMESTAMP -> Types.TIMESTAMP;
            });
        } else {
            statement.setObject(index, value);
        }
    }

    /** The column value as read back, typed like {@link #toColumn}. */
    static Object fromColumn(ResultSet rs, int index, Column column) throws SQLException {
        Object value = switch (column.kind()) {
            case TEXT, PASSWORD -> rs.getString(index);
            case LONG -> rs.getLong(index);
            case INT -> rs.getInt(index);
            case DECIMAL -> rs.getBigDecimal(index);
            case DATE -> rs.getObject(index, LocalDate.class);
            case TIMESTAMP -> rs.getObject(index, LocalDateTime.class);
        };
        return rs.wasNull() ? null : value;
    }

    /** Writes a column value back into its field the way a fixed-width extract would hold it. */
    static void toField(CobolRecord record, Column column, Object value) {
        if (column.field() instanceof NumericField numeric) {
            record.set(numeric, value == null ? BigDecimal.ZERO : new BigDecimal(value.toString()));
            return;
        }
        AlphanumericField alpha = (AlphanumericField) column.field();
        String text = switch (column.kind()) {
            case DATE -> value == null ? "" : value.toString();
            case TIMESTAMP -> value == null ? "" : FILE_TIMESTAMP.format((LocalDateTime) value);
            default -> value == null ? "" : value.toString();
        };
        record.set(alpha, text);
    }
}
