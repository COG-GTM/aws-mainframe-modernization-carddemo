package com.carddemo.online.comen01c;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.common.copybook.CardDemoCommarea;
import com.carddemo.runtime.cics.AidKey;
import com.carddemo.runtime.cics.CicsContext;
import com.carddemo.runtime.cics.CicsControlTransfer;
import com.carddemo.runtime.cobol.PicX;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.junit.jupiter.api.Test;

/**
 * Compares the Java paragraphs with golden values produced by running the COMEN01C statements under GnuCOBOL
 * (tools/gnucobol/comen01c/run.sh copies lines 35-48, 117-125, 127-129, 163-167, 172-176 and 269-272
 * verbatim around the real COMEN02Y copybook). Line kinds: P = option parsing + validity, N = 'not installed'
 * STRING, D = 'coming soon' STRING, B = BUILD-MENU-OPTIONS STRING. Entry 12 is 'Card Rewards' (the driver
 * fills the spare OCCURS entry).
 */
class Comen01cGnuCobolGoldenTest {

    private static final Pattern PARSE = Pattern.compile("P\\|\\[(..)]\\[(..)]\\[(..)]\\[(..)]\\|([VI])");
    private static final Pattern TEXT = Pattern.compile("([NDB])\\|(\\d\\d)\\|\\[(.*)]");
    private static final Clock CLOCK = Clock.fixed(Instant.parse("2024-03-05T14:07:09Z"), ZoneOffset.UTC);

    private static List<String> golden() throws IOException {
        try (InputStream in = Comen01cGnuCobolGoldenTest.class.getResourceAsStream(
                "/golden/comen01c/comen01c-golden.txt")) {
            assertThat(in).as("golden file").isNotNull();
            return new String(in.readAllBytes(), StandardCharsets.ISO_8859_1).lines().toList();
        }
    }

    @Test
    void optionParsingMatchesGnuCobol() throws IOException {
        int checked = 0;
        for (String line : golden()) {
            Matcher m = PARSE.matcher(line);
            if (!m.matches()) {
                continue;
            }
            CardDemoCommarea c = new CardDemoCommarea();
            c.setUserType("U");
            c.setPgmContext(1);
            Comen01c.Task task = new Comen01c.Task(CicsContext.builder().aid(AidKey.ENTER).commarea(c.toBytes())
                    .clock(CLOCK).build(), MenuOptions.comen02y());
            task.commarea.setBytes(c.toBytes());
            task.comen1a.setData("OPTION", m.group(1));
            try {
                task.processEnterKey();
            } catch (CicsControlTransfer xctl) {
                // valid option: XCTL to the menu program
            }
            assertThat(task.wsOptionX).as("WS-OPTION-X for " + line).isEqualTo(m.group(2));
            assertThat(task.wsOption).as("WS-OPTION for " + line).isEqualTo(m.group(3));
            assertThat(task.comen1a.data("OPTION")).as("OPTIONO for " + line).isEqualTo(m.group(4));
            assertThat(task.errFlgOn() ? "I" : "V").as("validity for " + line).isEqualTo(m.group(5));
            checked++;
        }
        assertThat(checked).isEqualTo(14);
    }

    @Test
    void stringStatementsMatchGnuCobol() throws IOException {
        List<MenuOptions.Entry> entries = new ArrayList<>(MenuOptions.COMEN02Y_ENTRIES);
        entries.add(new MenuOptions.Entry(12, "Card Rewards", PicX.lowValues(8), PicX.lowValues(1)));
        MenuOptions table = MenuOptions.of(11, entries);
        int checked = 0;
        for (String line : golden()) {
            Matcher m = TEXT.matcher(line);
            if (!m.matches()) {
                continue;
            }
            int i = Integer.parseInt(m.group(2));
            String expected = switch (m.group(1)) {
                case "N" -> Comen01c.Task.stringInto(PicX.spaces(80), "This option ",
                        Comen01c.Task.delimitedBy(table.name(i), "  "), " is not installed...");
                case "D" -> Comen01c.Task.stringInto(PicX.spaces(80), "This option ",
                        Comen01c.Task.delimitedBy(table.name(i), " "), "is coming soon ...");
                default -> Comen01c.Task.stringInto(PicX.spaces(40), table.num(i), ". ", table.name(i));
            };
            assertThat(expected).as(line).isEqualTo(m.group(3));
            checked++;
        }
        assertThat(checked).isEqualTo(36);
    }
}
