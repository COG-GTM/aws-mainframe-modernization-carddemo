/**
 * Transaction domain: {@code TRANSACT} (copybook {@code CVTRA05Y}), transaction types/categories,
 * category balances and disclosure groups. Online: {@code COTRN00C} (CT00), {@code COTRN01C} (CT01),
 * {@code COTRN02C} (CT02), {@code COBIL00C} (CB00).
 *
 * <p>Allowed dependencies: {@code common}, {@code account}, {@code card}, {@code customer} (ADR-0001, enforced by {@code ModularMonolithRules}). Types that other domains
 * must not use go in the {@code internal} sub-package.
 */
package com.carddemo.transaction;
