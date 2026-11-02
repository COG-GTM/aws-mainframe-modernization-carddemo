package com.carddemo.auth.menu;

import java.util.List;

/** COMEN01C ({@code MAIN}, CM00) or COADM01C ({@code ADMIN}, CA00) for the caller. */
public record MenuResponse(String menu, String transactionId, String program, String title,
                           List<MenuOption> options) {
}
