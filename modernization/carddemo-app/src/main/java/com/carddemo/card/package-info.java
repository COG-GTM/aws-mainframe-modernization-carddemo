/**
 * Card domain: {@code CARDDAT} (+ AIX {@code CARDAIX} on account id) and the card cross-reference {@code CCXREF}
 * (+ AIX {@code CXACAIX}, copybook {@code CVACT03Y}). Online: {@code COCRDLIC} (CCLI), {@code COCRDSLC} (CCDL),
 * {@code COCRDUPC} (CCUP).
 *
 * <p>Allowed dependencies: {@code common} only (ADR-0001, enforced by {@code ModularMonolithRules}). Types that other domains
 * must not use go in the {@code internal} sub-package.
 */
package com.carddemo.card;
