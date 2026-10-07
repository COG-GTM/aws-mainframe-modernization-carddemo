package com.carddemo.user.menu;

import com.carddemo.user.UserType;
import java.util.List;
import java.util.Optional;
import org.springframework.stereotype.Component;

/** The two menus of the core estate, with the option tables of {@code COMEN02Y} and {@code COADM02Y}. */
@Component
public class MenuCatalog {

    /** {@code COMEN02Y} ({@code CDEMO-MENU-OPT-COUNT = 11}). */
    public static final MenuDefinition MAIN = new MenuDefinition("main", "COMEN01C", "CM00", "COMEN01", "COMEN1A",
            12, true, List.of(
                    new MenuOption(1, "Account View", "COACTVWC", UserType.USER),
                    new MenuOption(2, "Account Update", "COACTUPC", UserType.USER),
                    new MenuOption(3, "Credit Card List", "COCRDLIC", UserType.USER),
                    new MenuOption(4, "Credit Card View", "COCRDSLC", UserType.USER),
                    new MenuOption(5, "Credit Card Update", "COCRDUPC", UserType.USER),
                    new MenuOption(6, "Transaction List", "COTRN00C", UserType.USER),
                    new MenuOption(7, "Transaction View", "COTRN01C", UserType.USER),
                    new MenuOption(8, "Transaction Add", "COTRN02C", UserType.USER),
                    new MenuOption(9, "Transaction Reports", "CORPT00C", UserType.USER),
                    new MenuOption(10, "Bill Payment", "COBIL00C", UserType.USER),
                    new MenuOption(11, "Pending Authorization View", "COPAUS0C", UserType.USER)));

    /** {@code COADM02Y} ({@code CDEMO-ADMIN-OPT-COUNT = 6}). */
    public static final MenuDefinition ADMIN = new MenuDefinition("admin", "COADM01C", "CA00", "COADM01", "COADM1A",
            12, false, List.of(
                    new MenuOption(1, "User List (Security)", "COUSR00C", null),
                    new MenuOption(2, "User Add (Security)", "COUSR01C", null),
                    new MenuOption(3, "User Update (Security)", "COUSR02C", null),
                    new MenuOption(4, "User Delete (Security)", "COUSR03C", null),
                    new MenuOption(5, "Transaction Type List/Update (Db2)", "COTRTLIC", null),
                    new MenuOption(6, "Transaction Type Maintenance (Db2)", "COTRTUPC", null)));

    private final MenuDefinition main;
    private final MenuDefinition admin;

    public MenuCatalog() {
        this(MAIN, ADMIN);
    }

    public MenuCatalog(MenuDefinition main, MenuDefinition admin) {
        this.main = main;
        this.admin = admin;
    }

    public MenuDefinition main() {
        return main;
    }

    public MenuDefinition admin() {
        return admin;
    }

    /** COSGN00C R-10/R-11: admin menu for type {@code 'A'}, main menu for everyone else. */
    public MenuDefinition forUserType(UserType userType) {
        return userType == UserType.ADMIN ? admin : main;
    }

    public Optional<MenuDefinition> byKey(String key) {
        if (main.key().equals(key)) {
            return Optional.of(main);
        }
        if (admin.key().equals(key)) {
            return Optional.of(admin);
        }
        return Optional.empty();
    }
}
