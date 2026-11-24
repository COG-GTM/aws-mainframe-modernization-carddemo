package com.carddemo.identity;

import java.util.List;

/** nextProgram keeps the legacy XCTL target for traceability; clients route on userType/menu. */
public record SignOnResponse(String accessToken, String tokenType, long expiresIn, String userId, String userType,
                             String firstName, String lastName, String nextProgram, List<MenuOption> menu) {
}
