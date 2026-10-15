package com.carddemo.identity;

import com.carddemo.common.web.KeysetPage;
import java.util.Map;
import org.springframework.http.HttpStatus;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/v1/users")
@PreAuthorize("hasRole('ADMIN')")
public class UserAdminController {

    private final UserAdminService service;

    public UserAdminController(UserAdminService service) {
        this.service = service;
    }

    @GetMapping
    public KeysetPage<UserView> list(@RequestParam(required = false) String startKey,
                                     @RequestParam(required = false) String after,
                                     @RequestParam(required = false) String before,
                                     @RequestParam(defaultValue = "1") int page) {
        return service.list(startKey, after, before, page);
    }

    @GetMapping("/{userId}")
    public UserView get(@PathVariable String userId) {
        return service.get(userId);
    }

    @PostMapping
    @ResponseStatus(HttpStatus.CREATED)
    public Map<String, String> add(@RequestBody UserRequest request) {
        return Map.of("message", service.add(request));
    }

    @PutMapping("/{userId}")
    public Map<String, String> update(@PathVariable String userId, @RequestBody UserRequest request) {
        return Map.of("message", service.update(userId, request));
    }

    @DeleteMapping("/{userId}")
    public Map<String, String> delete(@PathVariable String userId) {
        return Map.of("message", service.delete(userId));
    }
}
