package com.carddemo.menu;

public record MenuOption(
        int number,
        String name,
        String legacyProgram,
        String route,
        boolean adminOnly,
        boolean installed) {
}
