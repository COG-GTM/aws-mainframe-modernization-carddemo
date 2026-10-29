package com.carddemo.authfrds.api;

import java.util.List;

public record ApiError(int status, String error, String ruleId, String message, Integer sqlCode, List<String> fields) {
}
