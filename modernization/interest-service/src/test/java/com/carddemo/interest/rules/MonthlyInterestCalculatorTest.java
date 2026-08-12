package com.carddemo.interest.rules;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static org.junit.jupiter.api.Assertions.assertEquals;

/** Business-rule tests for BR-5 (monthly interest and its accumulation). */
class MonthlyInterestCalculatorTest {

    private final MonthlyInterestCalculator calculator = new MonthlyInterestCalculator();

    @Test
    @DisplayName("Monthly interest is balance x annual rate percent / 1200")
    void monthlyInterest() {
        assertEquals(new BigDecimal("15.00"),
                calculator.monthlyInterest(new BigDecimal("1200.00"), new BigDecimal("15.00")));
    }

    @Test
    @DisplayName("Results are truncated toward zero, never rounded half up")
    void truncatesRatherThanRounds() {
        // 79.60 * 15 / 1200 = 0.995 exactly: COMPUTE without ROUNDED keeps 0.99.
        assertEquals(new BigDecimal("0.99"),
                calculator.monthlyInterest(new BigDecimal("79.60"), new BigDecimal("15.00")));
        // Negative balances truncate toward zero as well.
        assertEquals(new BigDecimal("-0.99"),
                calculator.monthlyInterest(new BigDecimal("-79.60"), new BigDecimal("15.00")));
    }

    @Test
    @DisplayName("A zero balance or a zero rate yields zero interest")
    void zeroCases() {
        assertEquals(new BigDecimal("0.00"),
                calculator.monthlyInterest(new BigDecimal("0.00"), new BigDecimal("15.00")));
        assertEquals(new BigDecimal("0.00"),
                calculator.monthlyInterest(new BigDecimal("1234.56"), new BigDecimal("0.00")));
    }

    @Test
    @DisplayName("Accumulation keeps the PIC S9(09)V99 capacity of WS-TOTAL-INT")
    void accumulation() {
        assertEquals(new BigDecimal("25.75"),
                calculator.accumulate(new BigDecimal("10.25"), new BigDecimal("15.50")));
    }
}
