package com.carddemo.identity;

import com.carddemo.common.security.CardDemoPrincipal;
import java.util.List;

/** Static menu tables from copybooks COMEN02Y (main menu, COMEN01C) and COADM02Y (admin menu, COADM01C). */
public final class Menus {

    public static final List<MenuOption> MAIN = List.of(
            new MenuOption(1, "Account View", "COACTVWC", "GET", "/api/v1/accounts/{acctId}"),
            new MenuOption(2, "Account Update", "COACTUPC", "PUT", "/api/v1/accounts/{acctId}"),
            new MenuOption(3, "Credit Card List", "COCRDLIC", "GET", "/api/v1/cards"),
            new MenuOption(4, "Credit Card View", "COCRDSLC", "GET", "/api/v1/cards/{cardNum}"),
            new MenuOption(5, "Credit Card Update", "COCRDUPC", "PUT", "/api/v1/cards/{cardNum}"),
            new MenuOption(6, "Transaction List", "COTRN00C", "GET", "/api/v1/transactions"),
            new MenuOption(7, "Transaction View", "COTRN01C", "GET", "/api/v1/transactions/{tranId}"),
            new MenuOption(8, "Transaction Add", "COTRN02C", "POST", "/api/v1/transactions"),
            new MenuOption(9, "Transaction Reports", "CORPT00C", "POST", "/api/v1/reports/transactions"),
            new MenuOption(10, "Bill Payment", "COBIL00C", "POST", "/api/v1/bill-payments"),
            new MenuOption(11, "Pending Authorization View", "COPAUS0C", "GET",
                    "/api/v1/authorizations (phase 2: IMS/DB2/MQ module)"));

    public static final List<MenuOption> ADMIN = List.of(
            new MenuOption(1, "User List (Security)", "COUSR00C", "GET", "/api/v1/users"),
            new MenuOption(2, "User Add (Security)", "COUSR01C", "POST", "/api/v1/users"),
            new MenuOption(3, "User Update (Security)", "COUSR02C", "PUT", "/api/v1/users/{userId}"),
            new MenuOption(4, "User Delete (Security)", "COUSR03C", "DELETE", "/api/v1/users/{userId}"),
            new MenuOption(5, "Transaction Type List/Update (Db2)", "COTRTLIC", "GET", "/api/v1/transaction-types"),
            new MenuOption(6, "Transaction Type Maintenance (Db2)", "COTRTUPC", "PUT",
                    "/api/v1/transaction-types/{typeCd}"));

    private Menus() {
    }

    public static List<MenuOption> forUser(CardDemoPrincipal principal) {
        return principal.isAdmin() ? ADMIN : MAIN;
    }
}
