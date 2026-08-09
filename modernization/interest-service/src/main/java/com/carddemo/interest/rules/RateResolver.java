package com.carddemo.interest.rules;

import com.carddemo.interest.domain.TransactionCategory;

import java.math.BigDecimal;

/**
 * Resolves the annual interest rate (in percent) that applies to one account's balance in one
 * transaction category.
 *
 * <p>Business rule BR-3. Isolated behind an interface because rate sourcing is the part of the
 * interest cycle most likely to be replaced first (a pricing service, a rate table in a relational
 * database) once the sliver is running off-mainframe.
 */
public interface RateResolver {

    /**
     * @param accountGroupId the account's pricing group, possibly blank
     * @param category       the transaction type/category pair being priced
     * @return the disclosed annual rate in percent, e.g. {@code 15.00} for 15% APR
     */
    BigDecimal annualRatePercent(String accountGroupId, TransactionCategory category);
}
