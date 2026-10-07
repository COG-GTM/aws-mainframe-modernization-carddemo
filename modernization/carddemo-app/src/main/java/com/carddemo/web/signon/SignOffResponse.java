package com.carddemo.web.signon;

import io.swagger.v3.oas.annotations.media.Schema;

/** PF3 on the sign-on screen (COSGN00C R-3): the thank-you text; the client discards its token. */
@Schema(description = "Sign-off text (COSGN00C R-3)")
public record SignOffResponse(@Schema(example = "Thank you for using CardDemo application...") String message) {
}
