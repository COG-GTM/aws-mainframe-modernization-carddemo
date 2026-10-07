/**
 * Customer domain: {@code CUSTDAT} (copybook {@code CVCUS01Y}, RECLN 500); read by {@code CBCUS01C} (READCUST)
 * and, through the account domain, by {@code COACTVWC}/{@code COACTUPC}.
 *
 * <p>Allowed dependencies: {@code common} only (ADR-0001, enforced by {@code ModularMonolithRules}). Types that other domains
 * must not use go in the {@code internal} sub-package.
 */
package com.carddemo.customer;
