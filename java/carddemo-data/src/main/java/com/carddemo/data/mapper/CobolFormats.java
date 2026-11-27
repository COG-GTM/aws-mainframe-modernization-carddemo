package com.carddemo.data.mapper;

import com.carddemo.data.entity.UserType;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.ResolverStyle;

/**
 * Conversions between COBOL text representations and Java types, used by the MapStruct mappers.
 * Blank (all-spaces) items map to {@code null} and back to spaces.
 */
public final class CobolFormats {

    /** PIC X(10) dates such as ACCT-OPEN-DATE: {@code YYYY-MM-DD}. */
    public static final DateTimeFormatter DATE = DateTimeFormatter.ofPattern("uuuu-MM-dd")
            .withResolverStyle(ResolverStyle.STRICT);

    /** PIC X(26) DB2-style timestamps such as TRAN-ORIG-TS: {@code YYYY-MM-DD HH:MM:SS.ffffff}. */
    public static final DateTimeFormatter TIMESTAMP = DateTimeFormatter.ofPattern("uuuu-MM-dd HH:mm:ss.SSSSSS")
            .withResolverStyle(ResolverStyle.STRICT);

    private CobolFormats() {
    }

    public static LocalDate toLocalDate(String text) {
        return text == null || text.isBlank() ? null : LocalDate.parse(text.strip(), DATE);
    }

    public static String fromLocalDate(LocalDate date) {
        return date == null ? null : DATE.format(date);
    }

    public static LocalDateTime toLocalDateTime(String text) {
        return text == null || text.isBlank() ? null : LocalDateTime.parse(text.strip(), TIMESTAMP);
    }

    public static String fromLocalDateTime(LocalDateTime timestamp) {
        return timestamp == null ? null : TIMESTAMP.format(timestamp);
    }

    public static UserType toUserType(String code) {
        return UserType.fromCode(code);
    }

    public static String fromUserType(UserType type) {
        return type == null ? null : type.code();
    }
}
