package com.carddemo.user;

import com.carddemo.common.ApiException;
import com.carddemo.common.ErrorCode;
import com.carddemo.common.PageQuery;
import com.carddemo.common.PageResponse;
import com.carddemo.common.Text;
import com.carddemo.common.ValidationErrors;
import com.carddemo.user.UserDtos.CreateUserRequest;
import com.carddemo.user.UserDtos.UpdateUserRequest;
import com.carddemo.user.UserDtos.UserDetail;
import com.carddemo.user.UserDtos.UserSummary;
import java.util.List;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/** COUSR00C (list), COUSR01C (add), COUSR02C (update), COUSR03C (delete) over user_security. */
@Service
public class UserService {

    static final int DEFAULT_PAGE_SIZE = 10;

    private final UserRepository users;
    private final PasswordEncoder passwordEncoder;

    public UserService(UserRepository users, PasswordEncoder passwordEncoder) {
        this.users = users;
        this.passwordEncoder = passwordEncoder;
    }

    public PageResponse<UserSummary> list(String startKey, String direction, Integer pageSize) {
        PageQuery query = PageQuery.of(startKey == null ? null : Text.upperTrim(startKey), direction, pageSize,
                DEFAULT_PAGE_SIZE, "COUSR00C");
        PageResponse<UserRecord> page = users.page(query);
        List<UserSummary> items = page.items().stream()
                .map(u -> new UserSummary(u.userId(), u.firstName(), u.lastName(), u.userType()))
                .toList();
        String message = null;
        if (items.isEmpty() && query.startKey() != null) {
            message = query.direction() == PageQuery.Direction.NEXT
                    ? "You have reached the bottom of the page..."
                    : "You have reached the top of the page...";
        }
        return new PageResponse<>(items, page.firstKey(), page.lastKey(), page.hasNext(), page.hasPrev(), message);
    }

    public UserDetail get(String userId, String program) {
        if (Text.isBlank(userId)) {
            throw ApiException.validation(program, "userId", "User ID can NOT be empty...");
        }
        UserRecord user = users.findById(Text.upperTrim(userId))
                .orElseThrow(() -> ApiException.notFound(program, "User ID NOT found..."));
        return toDetail(user, null);
    }

    @Transactional
    public UserDetail create(CreateUserRequest request) {
        ValidationErrors errors = new ValidationErrors("COUSR01C");
        if (request == null || Text.isBlank(request.firstName())) {
            errors.add("firstName", "First Name can NOT be empty...");
        } else if (Text.isBlank(request.lastName())) {
            errors.add("lastName", "Last Name can NOT be empty...");
        } else if (Text.isBlank(request.userId())) {
            errors.add("userId", "User ID can NOT be empty...");
        } else if (Text.isBlank(request.password())) {
            errors.add("password", "Password can NOT be empty...");
        } else if (Text.isBlank(request.userType())) {
            errors.add("userType", "User Type can NOT be empty...");
        }
        errors.throwIfAny();
        checkLengthsAndType(errors, request.userId(), request.firstName(), request.lastName(), request.password(),
                request.userType());
        errors.throwIfAny();

        String userId = Text.upperTrim(request.userId());
        UserRecord user = new UserRecord(userId, request.firstName().strip(), request.lastName().strip(),
                passwordEncoder.encode(Text.upperTrim(request.password())), Text.upperTrim(request.userType()), 0);
        try {
            users.insert(user);
        } catch (DuplicateKeyException ex) {
            throw new ApiException(ErrorCode.DUPLICATE, "User ID already exist...", "COUSR01C");
        }
        return toDetail(user, "User " + userId + " has been added ...");
    }

    @Transactional
    public UserDetail update(String userIdIn, UpdateUserRequest request) {
        String program = "COUSR02C";
        ValidationErrors errors = new ValidationErrors(program);
        if (Text.isBlank(userIdIn)) {
            errors.add("userId", "User ID can NOT be empty...");
        } else if (request == null || Text.isBlank(request.firstName())) {
            errors.add("firstName", "First Name can NOT be empty...");
        } else if (Text.isBlank(request.lastName())) {
            errors.add("lastName", "Last Name can NOT be empty...");
        } else if (Text.isBlank(request.userType())) {
            errors.add("userType", "User Type can NOT be empty...");
        }
        errors.throwIfAny();
        checkLengthsAndType(errors, userIdIn, request.firstName(), request.lastName(), request.password(),
                request.userType());
        errors.throwIfAny();
        if (request.version() == null) {
            throw ApiException.validation(program, "version", "version is required");
        }

        UserRecord existing = users.findById(Text.upperTrim(userIdIn))
                .orElseThrow(() -> ApiException.notFound(program, "User ID NOT found..."));
        String firstName = request.firstName().strip();
        String lastName = request.lastName().strip();
        String userType = Text.upperTrim(request.userType());
        boolean passwordChanged = !Text.isBlank(request.password())
                && !passwordEncoder.matches(Text.upperTrim(request.password()), existing.passwordHash());
        boolean modified = !firstName.equals(existing.firstName()) || !lastName.equals(existing.lastName())
                || !userType.equals(existing.userType()) || passwordChanged;
        if (!modified) {
            throw ApiException.businessRule(program, "Please modify to update ...");
        }
        if (existing.version() != request.version()) {
            throw ApiException.concurrentUpdate(program);
        }
        String hash = passwordChanged ? passwordEncoder.encode(Text.upperTrim(request.password()))
                : existing.passwordHash();
        UserRecord updated = new UserRecord(existing.userId(), firstName, lastName, hash, userType,
                existing.version() + 1);
        if (users.update(updated, request.version()) == 0) {
            throw ApiException.concurrentUpdate(program);
        }
        return toDetail(updated, "User " + existing.userId() + " has been updated ...");
    }

    @Transactional
    public String delete(String userIdIn) {
        String program = "COUSR03C";
        if (Text.isBlank(userIdIn)) {
            throw ApiException.validation(program, "userId", "User ID can NOT be empty...");
        }
        String userId = Text.upperTrim(userIdIn);
        if (users.delete(userId) == 0) {
            throw ApiException.notFound(program, "User ID NOT found...");
        }
        return "User " + userId + " has been deleted ...";
    }

    private static void checkLengthsAndType(ValidationErrors errors, String userId, String firstName,
            String lastName, String password, String userType) {
        if (Text.trimToEmpty(userId).length() > 8) {
            errors.add("userId", "User ID can not be longer than 8 characters...");
        }
        if (Text.trimToEmpty(firstName).length() > 20) {
            errors.add("firstName", "First Name can not be longer than 20 characters...");
        }
        if (Text.trimToEmpty(lastName).length() > 20) {
            errors.add("lastName", "Last Name can not be longer than 20 characters...");
        }
        if (Text.trimToEmpty(password).length() > 8) {
            errors.add("password", "Password can not be longer than 8 characters...");
        }
        String type = Text.upperTrim(userType);
        if (!type.equals("A") && !type.equals("U")) {
            errors.add("userType", "User Type must be A or U...");
        }
    }

    private static UserDetail toDetail(UserRecord user, String message) {
        return new UserDetail(user.userId(), user.firstName(), user.lastName(), user.userType(), user.version(),
                message);
    }
}
