package com.carddemo.user.admin;

import com.carddemo.common.AbendException;
import com.carddemo.common.InvalidRequestException;
import com.carddemo.common.data.KeysetPage;
import com.carddemo.common.online.ScreenInput;
import com.carddemo.user.UserSecurity;
import com.carddemo.user.UserSecurityRepository;
import java.util.List;
import java.util.function.Supplier;
import org.springframework.dao.DataAccessException;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

/** COUSR00C (CU00): ten users per page in SEC-USR-ID order, forward and backward (ADR-0011 keyset browse). */
@Component
public class UserListBrowse {

    public static final int PAGE_SIZE = UserSecurityRepository.COUSR00C_SCREEN_ROWS;
    public static final String MSG_TOP_OF_PAGE = "You are at the top of the page...";
    public static final String MSG_REACHED_BOTTOM = "You have reached the bottom of the page...";
    public static final String MSG_REACHED_TOP = "You have reached the top of the page...";
    public static final String MSG_ALREADY_TOP = "You are already at the top of the page...";
    public static final String MSG_ALREADY_BOTTOM = "You are already at the bottom of the page...";
    public static final String MSG_INVALID_SELECTION = "Invalid selection. Valid values are U and D";
    public static final String MSG_BAD_CURSOR = "Cursor must be a user id (up to 8 characters) from the page shown";

    private final UserSecurityRepository users;

    public UserListBrowse(UserSecurityRepository users) {
        this.users = users;
    }

    /**
     * One screen of the list.
     *
     * @param rows        up to ten users in ascending SEC-USR-ID order
     * @param pageNumber  {@code PAGENUM} ({@code CDEMO-CU00-PAGE-NUM}); 0 when nothing was shown
     * @param hasPrevious PF7 would show an earlier page ({@code PAGE-NUM > 1})
     * @param hasNext     PF8 would show a later page ({@code NEXT-PAGE-YES})
     * @param message     {@code ERRMSG}, blank when none
     */
    public record Screen(List<UserSecurity> rows, int pageNumber, boolean hasPrevious, boolean hasNext,
            String message) {
    }

    /** What a {@code SELnnnn} code asks for (R-10/R-11). */
    public enum Action {
        UPDATE, DELETE;

        /** {@code U}/{@code u} → update, {@code D}/{@code d} → delete, else R-12. */
        public static Action of(String selection, String field) {
            String code = selection.strip();
            if (code.equalsIgnoreCase("U")) {
                return UPDATE;
            }
            if (code.equalsIgnoreCase("D")) {
                return DELETE;
            }
            throw new InvalidRequestException(field, MSG_INVALID_SELECTION);
        }
    }

    /**
     * ENTER ({@code startUserId}, no cursor; R-13), PF8 ({@code after} = the last id shown; R-16) or PF7
     * ({@code before} = the first id shown; R-14). {@code page} is the {@code PAGENUM} of the screen shown.
     */
    @Transactional(readOnly = true)
    public Screen browse(String startUserId, String after, String before, Integer page) {
        if (before != null) {
            return backward(cursor(before, "before"), page);
        }
        if (after != null) {
            return forward(cursor(after, "after"), page == null ? 1 : page);
        }
        String key = ScreenInput.isSpacesOrLowValues(startUserId) ? "" : ScreenInput.rightTrim(startUserId);
        KeysetPage<UserSecurity> first = read(() -> users.browseFrom(key));
        if (first.isEmpty()) {
            return new Screen(List.of(), 0, false, false, MSG_TOP_OF_PAGE);
        }
        return new Screen(first.rows(), 1, false, first.more(), first.more() ? "" : MSG_REACHED_BOTTOM);
    }

    /** {@code PROCESS-PF8-KEY} / {@code PROCESS-PAGE-FORWARD}, R-16/R-17/R-25: the users after the last one shown. */
    private Screen forward(String lastShown, int page) {
        KeysetPage<UserSecurity> next = read(() -> users.nextPage(lastShown));
        if (next.isEmpty()) {
            return new Screen(List.of(), page, page > 1, false, MSG_ALREADY_BOTTOM);
        }
        return new Screen(next.rows(), page + 1, true, next.more(), next.more() ? "" : MSG_REACHED_BOTTOM);
    }

    /**
     * {@code PROCESS-PF7-KEY} / {@code PROCESS-PAGE-BACKWARD}, R-14/R-15/R-23/R-26: PF7 on page 1 (or with nothing
     * before) re-shows the page from the first id shown.
     */
    private Screen backward(String firstShown, Integer page) {
        KeysetPage<UserSecurity> previous = page != null && page <= 1 ? new KeysetPage<>(List.of(), false)
                : read(() -> users.previousPage(firstShown));
        if (previous.isEmpty()) {
            KeysetPage<UserSecurity> same = read(() -> users.browseFrom(firstShown));
            return new Screen(same.rows(), same.isEmpty() ? 0 : 1, false, same.more(), MSG_ALREADY_TOP);
        }
        int number = page == null ? (previous.more() ? 2 : 1) : Math.max(1, page - 1);
        return new Screen(previous.rows(), number, previous.more(), true, previous.more() ? "" : MSG_REACHED_TOP);
    }

    private static String cursor(String value, String field) {
        String typed = ScreenInput.rightTrim(value);
        if (typed.isBlank() || typed.length() > 8) {
            throw new InvalidRequestException(field, MSG_BAD_CURSOR);
        }
        return typed;
    }

    /** R-24..R-26 other RESP: {@code Unable to lookup User...}. */
    private static <T> T read(Supplier<T> read) {
        try {
            return read.get();
        } catch (DataAccessException e) {
            throw AbendException.carddemo(UserAdminMessages.MSG_LOOKUP_FAILED, e);
        }
    }
}
