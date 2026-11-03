package com.carddemo.auth.flow;

import static java.util.Map.entry;

import com.carddemo.testsupport.flow.CobolMessages;
import com.carddemo.testsupport.flow.FlowCoverage;
import java.nio.file.Path;
import java.util.Map;
import org.junit.jupiter.api.Test;

/** Gate g-online-tests for auth-service, and the COBOL message texts its flow tests assert. */
class AuthFlowCoverageTest {

    private static final Path[] SOURCES = {
            CobolMessages.source(SignonAndMenuFlowTest.class), CobolMessages.source(UserAdministrationFlowTest.class)};

    private static final String SCREEN_STATE = "screen state / confirmation prompt; the API request is the confirmation";
    private static final String IO_ERROR = "CICS RESP other than NORMAL/NOTFND/DUPREC: an unhandled exception, 500";
    private static final String PAGE_EDGE = "page-edge text; the API answers an empty page or 400 on page < 0 (api-style.md §4)";

    @Test
    void everyAuthServiceScreenKeyHasAFlowTest() {
        FlowCoverage.assertEveryScreenKeyCovered("auth-service", SignonAndMenuFlowTest.class,
                UserAdministrationFlowTest.class);
    }

    @Test
    void COSGN00C_messagesAreAsserted() {
        CobolMessages.assertAsserted("COSGN00C", Map.of("Unable to verify the User ...", IO_ERROR), SOURCES);
    }

    @Test
    void COMEN01C_and_COADM01C_messagesAreAsserted() {
        CobolMessages.assertAsserted("COMEN01C", Map.of(), SOURCES);
        CobolMessages.assertAsserted("COADM01C", Map.of(), SOURCES);
    }

    @Test
    void COUSR00C_to_COUSR03C_messagesAreAsserted() {
        CobolMessages.assertAsserted("COUSR00C", Map.ofEntries(
                entry("Invalid selection. Valid values are U and D",
                        "row selection (U/D) is client navigation to PUT/DELETE /api/v1/users/{userId}"),
                entry("Unable to lookup User...", IO_ERROR),
                entry("You are already at the bottom of the page...", PAGE_EDGE),
                entry("You are already at the top of the page...", PAGE_EDGE),
                entry("You are at the top of the page...", PAGE_EDGE),
                entry("You have reached the bottom of the page...", PAGE_EDGE),
                entry("You have reached the top of the page...", PAGE_EDGE)), SOURCES);
        CobolMessages.assertAsserted("COUSR01C", Map.of("Unable to Add User...", IO_ERROR), SOURCES);
        CobolMessages.assertAsserted("COUSR02C", Map.of(
                "Press PF5 key to save your updates ...", SCREEN_STATE,
                "Unable to Update User...", IO_ERROR,
                "Unable to lookup User...", IO_ERROR), SOURCES);
        CobolMessages.assertAsserted("COUSR03C", Map.of(
                "Press PF5 key to delete this user ...", SCREEN_STATE,
                "Unable to Update User...", IO_ERROR,
                "Unable to lookup User...", IO_ERROR), SOURCES);
    }
}
