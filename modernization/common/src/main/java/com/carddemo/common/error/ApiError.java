package com.carddemo.common.error;

public record ApiError(int status, String code, String message) {
}
