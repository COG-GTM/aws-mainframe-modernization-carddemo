package com.carddemo.runtime.cics;

import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.util.Collection;
import java.util.Locale;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * The PROGRAM resources installed in the region (what EXEC CICS INQUIRE PROGRAM answers from). A program
 * can be installed without being migrated: XCTL to it still only reports {@code nextProgram}.
 */
public record InstalledPrograms(Set<String> names) {

    /** Classpath copy of {@code app/csd/CARDDEMO.CSD} (see carddemo-runtime pom). */
    public static final String CARDDEMO_CSD = "carddemo/csd/CARDDEMO.CSD";

    private static final Pattern DEFINE_PROGRAM = Pattern.compile("DEFINE\\s+PROGRAM\\(\\s*([^)\\s]+)\\s*\\)");

    public InstalledPrograms {
        TreeSet<String> normalized = new TreeSet<>();
        names.forEach(n -> normalized.add(normalize(n)));
        names = Set.copyOf(normalized);
    }

    public static InstalledPrograms none() {
        return new InstalledPrograms(Set.of());
    }

    /** Every {@code DEFINE PROGRAM(name)} in CSD extract text. */
    public static InstalledPrograms fromCsd(String csdText) {
        Set<String> found = new TreeSet<>();
        Matcher m = DEFINE_PROGRAM.matcher(csdText);
        while (m.find()) {
            found.add(m.group(1));
        }
        return new InstalledPrograms(found);
    }

    /** The programs defined in the base CardDemo CSD ({@code app/csd/CARDDEMO.CSD}). */
    public static InstalledPrograms carddemoCsd() {
        try (InputStream in = InstalledPrograms.class.getClassLoader().getResourceAsStream(CARDDEMO_CSD)) {
            if (in == null) {
                throw new IllegalStateException("Missing classpath resource " + CARDDEMO_CSD);
            }
            return fromCsd(new String(in.readAllBytes(), StandardCharsets.ISO_8859_1));
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    /** This set plus {@code more} (e.g. programs of an optional extension's CSD group). */
    public InstalledPrograms with(Collection<String> more) {
        Set<String> all = new TreeSet<>(names);
        more.stream().filter(n -> n != null && !n.isBlank()).forEach(all::add);
        return new InstalledPrograms(all);
    }

    public boolean contains(String program) {
        return program != null && names.contains(normalize(program));
    }

    private static String normalize(String program) {
        return program.trim().toUpperCase(Locale.ROOT);
    }
}
