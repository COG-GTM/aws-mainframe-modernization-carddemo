package com.carddemo.account.validation;

import java.util.List;

/**
 * One field edit of COACTUPC {@code 1200-EDIT-MAP-INPUTS}, in COBOL order.
 *
 * @param field     JSON field of {@code AccountUpdateRequest} the edit covers
 * @param label     {@code WS-EDIT-VARIABLE-NAME} used in the messages
 * @param paragraph COBOL paragraph(s) performing the edit
 * @param messages  every message the edit can produce, verbatim
 */
public record EditRule(int order, String field, String label, String paragraph, List<String> messages) {
}
