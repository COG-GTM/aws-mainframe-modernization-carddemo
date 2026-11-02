package com.carddemo.auth.api;

import com.carddemo.auth.api.dto.UserRequest;
import com.carddemo.auth.api.dto.UserResponse;
import com.carddemo.auth.service.AuthService;
import com.carddemo.common.api.PageResponse;
import jakarta.validation.Valid;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.RestController;

/** CICS CU00-CU03 / COUSR00C-COUSR03C. */
@RestController
@Validated
@RequestMapping("/api/v1/users")
public class UserController {

    private static final int MAX_PAGE_SIZE = 200;

    private final AuthService authService;

    public UserController(AuthService authService) {
        this.authService = authService;
    }

    @GetMapping
    public PageResponse<UserResponse> list(@RequestParam(defaultValue = "0") @Min(0) int page,
                                           @RequestParam(defaultValue = "20") @Min(1) @Max(MAX_PAGE_SIZE) int size) {
        return PageResponse.of(authService.list(PageRequest.of(page, size, Sort.by("userId")))
                .map(UserResponse::from));
    }

    @GetMapping("/{userId}")
    public UserResponse get(@PathVariable String userId) {
        return UserResponse.from(authService.get(userId));
    }

    @PostMapping
    @ResponseStatus(HttpStatus.CREATED)
    public UserResponse create(@Valid @RequestBody UserRequest request) {
        return UserResponse.from(authService.create(request));
    }

    @PutMapping("/{userId}")
    public UserResponse update(@PathVariable String userId, @Valid @RequestBody UserRequest request) {
        return UserResponse.from(authService.update(userId, request));
    }

    @DeleteMapping("/{userId}")
    public ResponseEntity<Void> delete(@PathVariable String userId) {
        authService.delete(userId);
        return ResponseEntity.noContent().build();
    }
}
