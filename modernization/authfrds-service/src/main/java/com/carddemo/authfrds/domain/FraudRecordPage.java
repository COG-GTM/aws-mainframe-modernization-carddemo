package com.carddemo.authfrds.domain;

import java.util.List;

public record FraudRecordPage(List<FraudRecord> items, int page, int size, long total, boolean hasPrevious,
                              boolean hasNext) {
}
