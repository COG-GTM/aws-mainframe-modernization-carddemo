package com.carddemo.user.menu;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.common.codec.TestData;
import com.carddemo.user.UserType;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.junit.jupiter.api.Test;

/** The Java option tables are exactly {@code app/cpy/COMEN02Y.cpy} and {@code app/cpy/COADM02Y.cpy}. */
class MenuTablesTest {

    private static final Pattern FILLER = Pattern.compile(
            "FILLER\\s+PIC (?:9\\(02\\) VALUE (\\d+)|X\\(\\d+\\) VALUE\\s+'([^']*)')\\.");

    /** FILLER values of the copybook (sequence areas, columns 1-6 and 73-80, and comment lines dropped), grouped into rows. */
    static List<List<String>> rows(String copybook, int columns) throws IOException {
        StringBuilder source = new StringBuilder();
        for (String line : Files.readAllLines(TestData.resolve("app/cpy/" + copybook), StandardCharsets.US_ASCII)) {
            if (line.length() > 6 && line.charAt(6) == '*') {
                continue;
            }
            source.append(line, Math.min(line.length(), 6), Math.min(line.length(), 72)).append('\n');
        }
        List<String> values = new ArrayList<>();
        Matcher m = FILLER.matcher(source);
        while (m.find()) {
            values.add(m.group(1) != null ? String.valueOf(Integer.parseInt(m.group(1))) : m.group(2).stripTrailing());
        }
        assertThat(values.size() % columns).isZero();
        List<List<String>> rows = new ArrayList<>();
        for (int i = 0; i < values.size(); i += columns) {
            rows.add(values.subList(i, i + columns));
        }
        return rows;
    }

    @Test
    void mainMenuIsComen02y() throws IOException {
        List<List<String>> expected = rows("COMEN02Y.cpy", 4);
        List<List<String>> actual = MenuCatalog.MAIN.options().stream().map(o -> List.of(
                String.valueOf(o.number()), o.name(), o.programId(), o.userType().code())).toList();
        assertThat(actual).isEqualTo(expected).hasSize(11);
    }

    @Test
    void adminMenuIsCoadm02y() throws IOException {
        List<List<String>> expected = rows("COADM02Y.cpy", 3);
        List<List<String>> actual = MenuCatalog.ADMIN.options().stream().map(o -> List.of(
                String.valueOf(o.number()), o.name(), o.programId())).toList();
        assertThat(actual).isEqualTo(expected).hasSize(6);
        assertThat(MenuCatalog.MAIN.options()).allMatch(o -> o.userType() == UserType.USER);
    }
}
