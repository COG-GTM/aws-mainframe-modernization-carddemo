/**
 * Batch domain: one Spring Batch job per JCL job (POSTTRAN, INTCALC, TRANREPT, CREASTMT, ...), the in-app
 * scheduler flow job, and the report request screen {@code CORPT00C} (CR00) that submits TRANREPT.
 *
 * <p>Allowed dependencies: {@code common} and every other domain; no domain may depend on {@code batch} (ADR-0001, enforced by {@code ModularMonolithRules}). Types that other domains
 * must not use go in the {@code internal} sub-package.
 */
package com.carddemo.batch;
