package com.carddemo.authfrds.domain;

import java.math.BigDecimal;

public final class Fixtures {
    private Fixtures() {
    }

    /** time9c 849999877 -> 999999999 - 849999877 = 150000122 -> 15:00:00.122 */
    public static AuthorizationDetail detail(String card, String fraud) {
        return new AuthorizationDetail(73697L, 849999877L, "261029", "150000", card, "0100", "1128", "0100", "POS",
                "A1B2C3", "00", "0000", "000000", new BigDecimal("125.50"), new BigDecimal("125.50"), "5411", "USA",
                "05", "MERCH0000000001", "Demo Grocery", "Seattle", "WA", "98101", "TXN000000000001", "P", fraud,
                "        ");
    }

    public static FraudRequest request(String card, String action) {
        return new FraudRequest(1L, 1L, action, detail(card, " "));
    }
}
