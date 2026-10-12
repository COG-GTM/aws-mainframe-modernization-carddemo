package com.carddemo.posting;

import org.springframework.boot.context.properties.ConfigurationProperties;

import java.nio.file.Path;

/**
 * The DD statements of {@code POSTTRAN.jcl STEP15}, as configuration.
 *
 * @param encoding           EBCDIC (shipped datasets) or ASCII
 * @param dailyTransactions  DALYTRAN  input  AWS.M2.CARDDEMO.DALYTRAN.PS
 * @param cardXref           XREFFILE  input  AWS.M2.CARDDEMO.CARDXREF.VSAM.KSDS
 * @param accountMaster      ACCTFILE  I-O    AWS.M2.CARDDEMO.ACCTDATA.VSAM.KSDS (rewritten in place)
 * @param categoryBalances   TCATBALF  I-O    AWS.M2.CARDDEMO.TCATBALF.VSAM.KSDS (rewritten in place)
 * @param transactionMaster  TRANFILE  output AWS.M2.CARDDEMO.TRANSACT.VSAM.KSDS (OPEN OUTPUT: replaced)
 * @param rejects            DALYREJS  output AWS.M2.CARDDEMO.DALYREJS(+1), LRECL 430
 */
@ConfigurationProperties(prefix = "carddemo.posting")
public record PostingProperties(
        String encoding,
        Path dailyTransactions,
        Path cardXref,
        Path accountMaster,
        Path categoryBalances,
        Path transactionMaster,
        Path rejects) {
}
