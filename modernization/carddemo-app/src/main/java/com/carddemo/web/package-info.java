/**
 * Web/API layer of the online programs: REST controllers (one per CICS program/BMS map), Spring Security with the
 * stateless JWT that replaces the COMMAREA (ADR-0017), the {@link com.carddemo.web.NavigationContext} handed between
 * screens, and OpenAPI. It may use {@code common} and every domain package; no domain package and not
 * {@code common} may depend on it (ADR-0001, enforced by {@code ModularMonolithRules.noDomainDependsOnWeb}).
 */
package com.carddemo.web;
