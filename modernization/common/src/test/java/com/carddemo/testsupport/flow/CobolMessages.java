package com.carddemo.testsupport.flow;

import static org.assertj.core.api.Assertions.assertThat;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * The screen messages a CICS program puts on its message line with {@code MOVE '<text>' TO WS-MESSAGE},
 * read from {@code app/cbl}, and the check that a service's flow tests assert each of them (or say why
 * the API cannot produce it).
 */
public final class CobolMessages {

    public static final Path COBOL_DIR = Path.of("..", "..", "app", "cbl");
    private static final Pattern MOVE_TO_WS_MESSAGE = Pattern.compile("MOVE\\s+'([^']*)'\\s+TO\\s+WS-MESSAGE\\b");

    private CobolMessages() {
    }

    /** Distinct {@code MOVE '...' TO WS-MESSAGE} literals (trailing blanks removed), in source order. */
    public static Set<String> wsMessageLiterals(String program) {
        StringBuilder code = new StringBuilder();
        try {
            for (String line : Files.readAllLines(COBOL_DIR.resolve(program + ".cbl"), StandardCharsets.ISO_8859_1)) {
                if (line.length() > 6 && (line.charAt(6) == '*' || line.charAt(6) == '/')) {
                    continue;
                }
                code.append(line, Math.min(7, line.length()), Math.min(72, line.length())).append(' ');
            }
        } catch (IOException ex) {
            throw new UncheckedIOException(ex);
        }
        Set<String> literals = new LinkedHashSet<>();
        Matcher m = MOVE_TO_WS_MESSAGE.matcher(code);
        while (m.find()) {
            literals.add(m.group(1).stripTrailing());
        }
        return literals;
    }

    /**
     * Every {@code WS-MESSAGE} literal of {@code program} is either a string literal in one of the test
     * sources or a key of {@code notProducedByTheApi} (message → reason). Reasons that name a message the
     * program does not have also fail, so the list stays honest.
     */
    public static void assertAsserted(String program, Map<String, String> notProducedByTheApi, Path... testSources) {
        String tests = List.of(testSources).stream().map(CobolMessages::read).collect(Collectors.joining("\n"));
        Set<String> literals = wsMessageLiterals(program);
        Set<String> missing = new TreeSet<>();
        for (String literal : literals) {
            // The COBOL literal may end in blanks the comparison above stripped ('... option... ').
            boolean asserted = Pattern.compile('"' + Pattern.quote(literal) + " *\"").matcher(tests).find();
            if (!notProducedByTheApi.containsKey(literal) && !asserted) {
                missing.add(literal);
            }
        }
        Set<String> stale = new TreeSet<>(notProducedByTheApi.keySet());
        stale.removeAll(literals);
        assertThat(missing).as("%s WS-MESSAGE texts no flow test asserts", program).isEmpty();
        assertThat(stale).as("%s: exemptions for texts the program does not have", program).isEmpty();
    }

    /** {@code src/test/java} source of a test class, from the module directory surefire runs in. */
    public static Path source(Class<?> testClass) {
        return Path.of("src", "test", "java", testClass.getName().replace('.', '/') + ".java");
    }

    private static String read(Path file) {
        try {
            return Files.readString(file);
        } catch (IOException ex) {
            throw new UncheckedIOException(ex);
        }
    }
}
