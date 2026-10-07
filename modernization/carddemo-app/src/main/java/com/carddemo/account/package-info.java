/**
 * Account domain: {@code ACCTDAT} (copybook {@code CVACT01Y}, RECLN 300). Online: {@code COACTVWC} (CAVW),
 * {@code COACTUPC} (CAUP). Reads the card cross-reference ({@code CXACAIX}) and the customer record.
 *
 * <p>Allowed dependencies: {@code common}, {@code card}, {@code customer} (ADR-0001, enforced by {@code ModularMonolithRules}). Types that other domains
 * must not use go in the {@code internal} sub-package.
 */
package com.carddemo.account;
