package com.carddemo.auth;

import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/auth")
public class SignonController {

    private final SignonService signonService;

    public SignonController(SignonService signonService) {
        this.signonService = signonService;
    }

    @PostMapping("/signon")
    public SignonResponse signon(@RequestBody(required = false) SignonRequest request) {
        return signonService.signon(request);
    }
}
