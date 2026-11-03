package com.carddemo.testsupport.flow;

/**
 * The 3270 keys that make an online program do server-side work (api-style.md §1). PF3 (exit), PF4 /
 * PF12 (clear, cancel) are client navigation and need no flow test.
 */
public enum ScreenKey {
    /** ENTER: inquiry, add, or the first step of an update/delete (fetch the record). */
    ENTER,
    /** PF5: save (COACTUPC, COCRDUPC, COUSR02C), delete (COUSR03C), copy last transaction (COTRN02C). */
    PF5,
    /** PF7: page back. */
    PF7,
    /** PF8: page forward. */
    PF8
}
