package com.carddemo.programs.csutldtc;

import com.carddemo.programs.csutldtc.copybook.FeedbackCode;

/** OUTPUT-LILLIAN (PIC S9(9) BINARY) and FEEDBACK-CODE returned by CEEDAYS. */
public record CeedaysResult(int outputLillian, FeedbackCode feedbackCode) {
}
