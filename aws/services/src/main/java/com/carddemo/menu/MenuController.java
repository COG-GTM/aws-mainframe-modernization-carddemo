package com.carddemo.menu;

import com.carddemo.common.CardDemoProperties;
import java.util.List;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/** COMEN01C / COADM01C: option tables from COMEN02Y and COADM02Y. */
@RestController
@RequestMapping("/api/v1/menus")
public class MenuController {

    private final List<MenuOption> mainOptions;
    private final List<MenuOption> adminOptions;

    public MenuController(CardDemoProperties properties) {
        boolean authInstalled = properties.modules().authorizationsInstalled();
        this.mainOptions = List.of(
                new MenuOption(1, "Account View", "COACTVWC", "/accounts/view", false, true),
                new MenuOption(2, "Account Update", "COACTUPC", "/accounts/update", false, true),
                new MenuOption(3, "Credit Card List", "COCRDLIC", "/cards", false, true),
                new MenuOption(4, "Credit Card View", "COCRDSLC", "/cards/view", false, true),
                new MenuOption(5, "Credit Card Update", "COCRDUPC", "/cards/update", false, true),
                new MenuOption(6, "Transaction List", "COTRN00C", "/transactions", false, true),
                new MenuOption(7, "Transaction View", "COTRN01C", "/transactions/view", false, true),
                new MenuOption(8, "Transaction Add", "COTRN02C", "/transactions/new", false, true),
                new MenuOption(9, "Transaction Reports", "CORPT00C", "/reports", false, true),
                new MenuOption(10, "Bill Payment", "COBIL00C", "/bill-payment", false, true),
                new MenuOption(11, "Pending Authorization View", "COPAUS0C", "/authorizations",
                        false, authInstalled));
        this.adminOptions = List.of(
                new MenuOption(1, "User List (Security)", "COUSR00C", "/admin/users", true, true),
                new MenuOption(2, "User Add (Security)", "COUSR01C", "/admin/users/new", true, true),
                new MenuOption(3, "User Update (Security)", "COUSR02C", "/admin/users/:userId/edit", true, true),
                new MenuOption(4, "User Delete (Security)", "COUSR03C", "/admin/users/:userId/delete", true, true),
                new MenuOption(5, "Transaction Type List/Update (Db2)", "COTRTLIC",
                        "/admin/transaction-types", true, true),
                new MenuOption(6, "Transaction Type Maintenance (Db2)", "COTRTUPC",
                        "/admin/transaction-types/maintain", true, true));
    }

    @GetMapping("/main")
    public MenuResponse main() {
        return new MenuResponse(mainOptions);
    }

    @GetMapping("/admin")
    public MenuResponse admin() {
        return new MenuResponse(adminOptions);
    }
}
