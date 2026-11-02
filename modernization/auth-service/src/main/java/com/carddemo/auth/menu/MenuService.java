package com.carddemo.auth.menu;

import com.carddemo.common.error.BusinessRuleException;
import com.carddemo.common.error.ForbiddenException;
import com.carddemo.common.security.AdminOnlyInterceptor;
import com.carddemo.common.security.CallerIdentity;
import java.util.List;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.web.server.ResponseStatusException;

/** COMEN01C / COADM01C without the BMS map: build the option list, validate a selection. */
@Service
public class MenuService {

    static final String INVALID_OPTION = "Please enter a valid option number...";

    /** COSGN00C routing: CDEMO-USRTYP-ADMIN gets COADM01C, everyone else COMEN01C. */
    public MenuResponse menuFor(CallerIdentity caller) {
        if (caller.isAdmin()) {
            return new MenuResponse("ADMIN", "CA00", "COADM01C", "Admin Menu", MenuCatalog.ADMIN);
        }
        // COMEN01C BUILD-MENU-OPTIONS lists every row; the admin-only check happens only on selection.
        return new MenuResponse("MAIN", "CM00", "COMEN01C", "Main Menu", MenuCatalog.MAIN);
    }

    /**
     * COMEN01C/COADM01C PROCESS-ENTER-KEY: range check, the COMEN01C admin-only check (unreachable with
     * the shipped COMEN02Y, every option is 'U'), then the "not installed" branch; otherwise the option
     * whose program the COBOL would XCTL to.
     */
    public MenuOption select(CallerIdentity caller, int number) {
        List<MenuOption> options = caller.isAdmin() ? MenuCatalog.ADMIN : MenuCatalog.MAIN;
        if (number < 1 || number > options.size()) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, INVALID_OPTION);
        }
        MenuOption option = options.get(number - 1);
        if (option.adminOnly() && !caller.isAdmin()) {
            throw new ForbiddenException(AdminOnlyInterceptor.ADMIN_ONLY_MESSAGE);
        }
        if (!option.available()) {
            throw new BusinessRuleException(caller.isAdmin()
                    ? "This option is not installed ..."  // COADM01C leaves the name out
                    : "This option " + option.name() + " is not installed...");
        }
        return option;
    }
}
