package com.carddemo.card.flow;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.card.service.CardMessages;
import com.carddemo.testsupport.flow.CobolMessages;
import com.carddemo.testsupport.flow.FlowCoverage;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;

/**
 * Gate g-online-tests for card-service. The card programs set their messages through 88-level VALUEs,
 * not {@code MOVE '...' TO WS-MESSAGE}; the ones the API produces are {@link CardMessages}, and every one
 * of them must be asserted by a flow test.
 */
class CardFlowCoverageTest {

    @Test
    void everyCardServiceScreenKeyHasAFlowTest() {
        FlowCoverage.assertEveryScreenKeyCovered("card-service", CardFlowTest.class);
    }

    @Test
    void everyCardMessageIsAssertedByAFlowTest() throws Exception {
        String tests = Files.readString(CobolMessages.source(CardFlowTest.class));
        List<String> missing = new ArrayList<>();
        for (Field field : CardMessages.class.getDeclaredFields()) {
            if (Modifier.isStatic(field.getModifiers()) && field.getType() == String.class) {
                String message = (String) field.get(null);
                if (!tests.contains('"' + message + '"')) {
                    missing.add(field.getName() + " = " + message);
                }
            }
        }
        assertThat(missing).isEmpty();
    }
}
