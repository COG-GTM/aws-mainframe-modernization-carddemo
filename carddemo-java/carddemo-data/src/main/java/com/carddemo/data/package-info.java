/**
 * PostgreSQL persistence for the CardDemo datasets that were VSAM KSDS files (ACCTDAT, CARDDAT,
 * CUSTDAT, CCXREF, TRANSACT, ...). Schema lives in Flyway migrations under {@code db/migration};
 * {@code entity} maps one table per copybook and {@code repository} holds the keyed and alternate-index
 * (AIX) reads the COBOL programs perform.
 */
package com.carddemo.data;
