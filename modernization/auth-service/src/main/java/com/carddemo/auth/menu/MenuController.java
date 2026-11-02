package com.carddemo.auth.menu;

import com.carddemo.common.security.CallerIdentity;
import jakarta.servlet.http.HttpServletRequest;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/** CICS CM00 / CA00: the menu the signed-on user may see, chosen by the token's user type. */
@RestController
@RequestMapping("/api/v1/menu")
public class MenuController {

    private final MenuService menus;

    public MenuController(MenuService menus) {
        this.menus = menus;
    }

    @GetMapping
    public MenuResponse menu(HttpServletRequest request) {
        return menus.menuFor(caller(request));
    }

    @GetMapping("/options/{number}")
    public MenuOption select(@PathVariable int number, HttpServletRequest request) {
        return menus.select(caller(request), number);
    }

    private static CallerIdentity caller(HttpServletRequest request) {
        return CallerIdentity.from(request).orElseThrow(() -> new IllegalStateException("no caller"));
    }
}
