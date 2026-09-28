package com.carddemo.user;

import com.carddemo.common.PageResponse;
import com.carddemo.user.UserDtos.CreateUserRequest;
import com.carddemo.user.UserDtos.UpdateUserRequest;
import com.carddemo.user.UserDtos.UserDetail;
import com.carddemo.user.UserDtos.UserSummary;
import java.net.URI;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/users")
public class UserController {

    private final UserService service;

    public UserController(UserService service) {
        this.service = service;
    }

    @GetMapping
    public PageResponse<UserSummary> list(@RequestParam(required = false) String startKey,
            @RequestParam(required = false) String direction, @RequestParam(required = false) Integer pageSize) {
        return service.list(startKey, direction, pageSize);
    }

    @GetMapping("/{userId}")
    public UserDetail get(@PathVariable String userId) {
        return service.get(userId, "COUSR02C");
    }

    @PostMapping
    public ResponseEntity<UserDetail> create(@RequestBody(required = false) CreateUserRequest request) {
        UserDetail created = service.create(request);
        return ResponseEntity.created(URI.create("/api/v1/users/" + created.userId())).body(created);
    }

    @PutMapping("/{userId}")
    public UserDetail update(@PathVariable String userId, @RequestBody(required = false) UpdateUserRequest request) {
        return service.update(userId, request);
    }

    @DeleteMapping("/{userId}")
    public ResponseEntity<Void> delete(@PathVariable String userId) {
        service.delete(userId);
        return ResponseEntity.noContent().build();
    }
}
