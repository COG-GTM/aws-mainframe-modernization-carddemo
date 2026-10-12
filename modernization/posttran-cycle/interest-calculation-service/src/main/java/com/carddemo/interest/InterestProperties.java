package com.carddemo.interest;

import org.springframework.boot.context.properties.ConfigurationProperties;

import java.nio.file.Path;

/**
 * DD statements of INTCALC.jcl STEP15 plus the PARM.
 *
 * @param runDate          {@code PARM='2022071800'}: 10 characters used verbatim as the transaction id prefix
 * @param categoryBalances TCATBALF input (sequential browse in key order)
 * @param cardXref         XREFFILE / XREFFIL1 (read through the account-id alternate index)
 * @param accountMaster    ACCTFILE I-O
 * @param disclosureGroups DISCGRP input
 * @param systemTransactions TRANSACT output AWS.M2.CARDDEMO.SYSTRAN(+1), RECFM=F LRECL=350
 */
@ConfigurationProperties(prefix = "carddemo.interest")
public record InterestProperties(
        String encoding,
        String runDate,
        Path categoryBalances,
        Path cardXref,
        Path accountMaster,
        Path disclosureGroups,
        Path systemTransactions) {
}
