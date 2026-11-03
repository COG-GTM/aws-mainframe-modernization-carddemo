package com.carddemo.testsupport.flow;

import static org.assertj.core.api.Assertions.assertThat;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;

/** Gate g-online-tests: every screen key of every program a service owns has a flow test. */
public final class FlowCoverage {

    /** One {@link CobolFlow} test method. */
    public record Entry(String testClass, String method, String program, String paragraph, ScreenKey key) {
    }

    private FlowCoverage() {
    }

    public static List<Entry> entries(Class<?>... testClasses) {
        List<Entry> entries = new ArrayList<>();
        for (Class<?> type : testClasses) {
            for (Method method : type.getDeclaredMethods()) {
                CobolFlow flow = method.getAnnotation(CobolFlow.class);
                if (flow != null) {
                    entries.add(new Entry(type.getSimpleName(), method.getName(), flow.program(), flow.paragraph(),
                            flow.key()));
                }
            }
        }
        return entries;
    }

    /**
     * Fails unless every {@link OnlineScreens} key of {@code service} is covered by a {@link CobolFlow} test in
     * {@code testClasses}, every annotated method is a test whose name starts with
     * {@code <PROGRAM>_<PARAGRAPH>}, and every annotation names a program the service owns.
     */
    public static void assertEveryScreenKeyCovered(String service, Class<?>... testClasses) {
        List<Entry> entries = entries(testClasses);
        Set<String> problems = new TreeSet<>();
        for (Class<?> type : testClasses) {
            for (Method method : type.getDeclaredMethods()) {
                CobolFlow flow = method.getAnnotation(CobolFlow.class);
                if (flow == null) {
                    continue;
                }
                if (!method.isAnnotationPresent(Test.class) && !method.isAnnotationPresent(ParameterizedTest.class)) {
                    problems.add(method.getName() + ": @CobolFlow on a method that is not a test");
                }
                String prefix = flow.program() + "_" + flow.paragraph().replace('-', '_') + "_";
                if (!method.getName().startsWith(prefix)) {
                    problems.add(method.getName() + ": name must start with " + prefix);
                }
                boolean owned = OnlineScreens.find(flow.program()).map(s -> s.service().equals(service)).orElse(false);
                if (!owned) {
                    problems.add(method.getName() + ": " + flow.program() + " is not an online program of " + service);
                }
            }
        }
        for (OnlineScreens.Screen screen : OnlineScreens.ofService(service)) {
            for (ScreenKey key : screen.keys()) {
                boolean covered = entries.stream().anyMatch(e -> e.program().equals(screen.program()) && e.key() == key);
                if (!covered) {
                    problems.add(screen.program() + " " + key + ": no flow test");
                }
            }
        }
        assertThat(problems).as("online flow coverage of %s (%s)", service,
                Arrays.stream(testClasses).map(Class::getSimpleName).toList()).isEmpty();
    }
}
