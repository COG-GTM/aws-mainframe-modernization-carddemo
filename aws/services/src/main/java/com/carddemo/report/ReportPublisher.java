package com.carddemo.report;

/** Replaces CORPT00C's write of the TRANREPT JCL deck to TD queue JOBS. */
public interface ReportPublisher {

    void publish(ReportRequest request);
}
