package com.carddemo.identity;

/** One line of COMEN02Y / COADM02Y: option number, label, legacy program, and the REST resource that replaces it. */
public record MenuOption(int option, String name, String legacyProgram, String method, String path) {
}
