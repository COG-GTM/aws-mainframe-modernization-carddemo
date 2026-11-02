package com.carddemo.card.api;

import com.carddemo.card.service.CardMessages;
import jakarta.validation.Constraint;
import jakarta.validation.Payload;
import jakarta.validation.constraints.Pattern;
import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Card number key of COCRDSLC / COCRDUPC (2220-EDIT-CARD / 1220-EDIT-CARD): 16 digits. An all-zero
 * number counts as blank on the screen ({@code Card number not provided}).
 */
@Documented
@Constraint(validatedBy = {})
@Target({ElementType.PARAMETER, ElementType.FIELD})
@Retention(RetentionPolicy.RUNTIME)
@Pattern(regexp = "\\d{16}", message = CardMessages.CARD_FILTER_NOT_NUMERIC)
@Pattern(regexp = "(?!0{16}$).*", message = CardMessages.CARD_NOT_PROVIDED)
public @interface CardNumber {

    String message() default CardMessages.CARD_FILTER_NOT_NUMERIC;

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};
}
