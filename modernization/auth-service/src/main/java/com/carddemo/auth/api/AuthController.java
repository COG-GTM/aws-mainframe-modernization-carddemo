package com.carddemo.auth.api;

import com.carddemo.auth.api.dto.SignonRequest;
import com.carddemo.auth.api.dto.SignonResponse;
import com.carddemo.auth.service.AuthService;
import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/** CICS CC00 / COSGN00C. */
@RestController
@RequestMapping("/api/v1/auth")
public class AuthController {

    private final AuthService authService;

    public AuthController(AuthService authService) {
        this.authService = authService;
    }

    @PostMapping("/signon")
    public SignonResponse signon(@Valid @RequestBody SignonRequest request) {
        return authService.signon(request);
    }
}
