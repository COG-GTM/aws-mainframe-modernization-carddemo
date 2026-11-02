package com.carddemo.auth.menu;

import java.util.List;

/** The option tables of COMEN02Y and COADM02Y, in copybook order with copybook text. */
public final class MenuCatalog {

    /** COMEN02Y: CDEMO-MENU-OPT-COUNT = 11, every USRTYPE is 'U'. */
    public static final List<MenuOption> MAIN = List.of(
            new MenuOption(1, "Account View", "COACTVWC", false, true, "GET /api/v1/accounts/{accountId}"),
            new MenuOption(2, "Account Update", "COACTUPC", false, true, "PUT /api/v1/accounts/{accountId}"),
            new MenuOption(3, "Credit Card List", "COCRDLIC", false, true, "GET /api/v1/cards"),
            new MenuOption(4, "Credit Card View", "COCRDSLC", false, true, "GET /api/v1/cards/{cardNumber}"),
            new MenuOption(5, "Credit Card Update", "COCRDUPC", false, true, "PUT /api/v1/cards/{cardNumber}"),
            new MenuOption(6, "Transaction List", "COTRN00C", false, true, "GET /api/v1/transactions"),
            new MenuOption(7, "Transaction View", "COTRN01C", false, true,
                    "GET /api/v1/transactions/{transactionId}"),
            new MenuOption(8, "Transaction Add", "COTRN02C", false, true, "POST /api/v1/transactions"),
            new MenuOption(9, "Transaction Reports", "CORPT00C", false, true,
                    "POST /api/v1/reports/transactions"),
            new MenuOption(10, "Bill Payment", "COBIL00C", false, true, "POST /api/v1/bill-payments"),
            new MenuOption(11, "Pending Authorization View", "COPAUS0C", false, false, null));

    /** COADM02Y: CDEMO-ADMIN-OPT-COUNT = 6. */
    public static final List<MenuOption> ADMIN = List.of(
            new MenuOption(1, "User List (Security)", "COUSR00C", true, true, "GET /api/v1/users"),
            new MenuOption(2, "User Add (Security)", "COUSR01C", true, true, "POST /api/v1/users"),
            new MenuOption(3, "User Update (Security)", "COUSR02C", true, true, "PUT /api/v1/users/{userId}"),
            new MenuOption(4, "User Delete (Security)", "COUSR03C", true, true, "DELETE /api/v1/users/{userId}"),
            new MenuOption(5, "Transaction Type List/Update (Db2)", "COTRTLIC", true, false, null),
            new MenuOption(6, "Transaction Type Maintenance (Db2)", "COTRTUPC", true, false, null));

    private MenuCatalog() {
    }
}
