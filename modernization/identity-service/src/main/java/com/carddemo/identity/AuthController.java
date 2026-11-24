package com.carddemo.identity;

import com.carddemo.common.security.CardDemoPrincipal;
import com.carddemo.common.security.CurrentUser;
import java.util.List;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1")
public class AuthController {

    private final SignOnService signOnService;

    public AuthController(SignOnService signOnService) {
        this.signOnService = signOnService;
    }

    /** COSGN00C. */
    @PostMapping("/auth/signon")
    public SignOnResponse signOn(@RequestBody SignOnRequest request) {
        return signOnService.signOn(request);
    }

    /** COMEN01C / COADM01C. */
    @GetMapping("/menu")
    public List<MenuOption> menu() {
        CardDemoPrincipal principal = CurrentUser.get();
        return Menus.forUser(principal);
    }
}
