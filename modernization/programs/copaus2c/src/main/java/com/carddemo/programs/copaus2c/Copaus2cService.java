package com.carddemo.programs.copaus2c;

import com.carddemo.programs.copaus2c.copybook.AuthFrdsRow;
import com.carddemo.programs.copaus2c.copybook.FraudCommarea;
import com.carddemo.programs.copaus2c.copybook.FraudCommarea.FraudStatusRecord;
import com.carddemo.programs.copaus2c.copybook.FraudCommarea.FraudUpdateStatus;
import com.carddemo.programs.copaus2c.copybook.PendingAuthDetails;
import com.carddemo.programs.copaus2c.copybook.Pic;
import com.carddemo.programs.copaus2c.copybook.Sqlca;
import com.carddemo.programs.copaus2c.gateway.AuthFrdsGateway;

import java.time.Clock;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

/**
 * COPAUS2C - Mark Authorization Message Fraud. Invoked by COPAUS1C via EXEC CICS LINK with the
 * fraud COMMAREA; records (or re-flags) the authorization in CARDDEMO.AUTHFRDS and returns the
 * updated COMMAREA.
 */
public final class Copaus2cService {

    public static final String PROGRAM_NAME = "COPAUS2C";

    static final String ADD_SUCCESS = "ADD SUCCESS";
    static final String UPDT_SUCCESS = "UPDT SUCCESS";
    static final String SYSTEM_ERROR_PREFIX = " SYSTEM ERROR DB2: CODE:";
    static final String UPDT_ERROR_PREFIX = " UPDT ERROR DB2: CODE:";
    static final String STATE_LABEL = ", STATE: ";

    private static final DateTimeFormatter MMDDYY_SLASH = DateTimeFormatter.ofPattern("MM/dd/yy");

    private final AuthFrdsGateway authFrds;
    private final Clock clock;

    public Copaus2cService(AuthFrdsGateway authFrds, Clock clock) {
        this.authFrds = authFrds;
        this.clock = clock;
    }

    /** EXEC CICS LINK PROGRAM('COPAUS2C') COMMAREA(...): returns the COMMAREA as left by EXEC CICS RETURN. */
    public FraudCommarea execute(FraudCommarea commarea) {
        return mainPara(commarea);
    }

    // MAIN-PARA
    FraudCommarea mainPara(FraudCommarea commarea) {
        String wsCurDate = formatTimeMmddyy(LocalDate.now(clock));
        PendingAuthDetails auth = commarea.fraudAuthRecord().withFraudRptDate(wsCurDate);
        FraudStatusRecord status = commarea.fraudStatusRecord();

        AuthFrdsRow row = buildAuthFrdsRow(commarea.acctId(), commarea.custId(), auth, status.frdAction());

        Sqlca sqlca = authFrds.insert(row);
        if (sqlca.sqlcode() == 0) {
            status = new FraudStatusRecord(status.frdAction(), FraudUpdateStatus.WS_FRD_UPDT_SUCCESS.value,
                    ADD_SUCCESS);
        } else if (sqlca.sqlcode() == Sqlca.DUPLICATE_KEY) {
            status = fraudUpdate(row, status);
        } else {
            status = new FraudStatusRecord(status.frdAction(), FraudUpdateStatus.WS_FRD_UPDT_FAILED.value,
                    db2ErrorMessage(status.frdActMsg(), SYSTEM_ERROR_PREFIX, sqlca));
        }

        // EXEC CICS RETURN
        return new FraudCommarea(commarea.acctId(), commarea.custId(), auth, status);
    }

    // FRAUD-UPDATE
    FraudStatusRecord fraudUpdate(AuthFrdsRow row, FraudStatusRecord status) {
        Sqlca sqlca = authFrds.updateFraud(row.authFraud(), row.cardNum(), row.authTs());
        if (sqlca.sqlcode() == 0) {
            return new FraudStatusRecord(status.frdAction(), FraudUpdateStatus.WS_FRD_UPDT_SUCCESS.value,
                    UPDT_SUCCESS);
        }
        return new FraudStatusRecord(status.frdAction(), FraudUpdateStatus.WS_FRD_UPDT_FAILED.value,
                db2ErrorMessage(status.frdActMsg(), UPDT_ERROR_PREFIX, sqlca));
    }

    /** EXEC CICS ASKTIME + FORMATTIME MMDDYY(WS-CUR-DATE) DATESEP: 'MM/DD/YY'. */
    static String formatTimeMmddyy(LocalDate date) {
        return date.format(MMDDYY_SLASH);
    }

    /** Builds WS-AUTH-TS 'YY-MM-DD HH.MI.SSsss000' (23 chars) from the authorization key. */
    static String buildWsAuthTs(PendingAuthDetails auth) {
        String origDate = auth.authOrigDate();
        // COMPUTE WS-AUTH-TIME = 999999999 - PA-AUTH-TIME-9C  (WS-AUTH-TIME PIC 9(09))
        long wsAuthTime = Pic.unsigned(999_999_999L - auth.authTime9C(), 9);
        String wsAuthTimeAn = Pic.display9(wsAuthTime, 9);
        return Pic.refMod(origDate, 1, 2) + "-"
                + Pic.refMod(origDate, 3, 2) + "-"
                + Pic.refMod(origDate, 5, 2) + " "
                + Pic.refMod(wsAuthTimeAn, 1, 2) + "."
                + Pic.refMod(wsAuthTimeAn, 3, 2) + "."
                + Pic.refMod(wsAuthTimeAn, 5, 2)
                + Pic.refMod(wsAuthTimeAn, 7, 3)
                + "000";
    }

    /** The MOVE block that populates DCLAUTHFRDS before the INSERT. */
    static AuthFrdsRow buildAuthFrdsRow(long acctId, long custId, PendingAuthDetails auth, String frdAction) {
        return new AuthFrdsRow(
                auth.cardNum(),
                buildWsAuthTs(auth),
                auth.authType(),
                auth.cardExpiryDate(),
                auth.messageType(),
                auth.messageSource(),
                auth.authIdCode(),
                auth.authRespCode(),
                auth.authRespReason(),
                Pic.display9(auth.processingCode(), 6),
                auth.transactionAmt(),
                auth.approvedAmt(),
                auth.merchantCatagoryCode(),
                auth.acqrCountryCode(),
                (short) auth.posEntryMode(),
                auth.merchantId(),
                (short) PendingAuthDetails.MERCHANT_NAME_LENGTH,
                auth.merchantName(),
                auth.merchantCity(),
                auth.merchantState(),
                auth.merchantZip(),
                auth.transactionId(),
                auth.matchStatus(),
                frdAction,
                "",
                acctId,
                custId);
    }

    /**
     * MOVE SQLCODE TO WS-SQLCODE (+9(06)), MOVE SQLSTATE TO WS-SQLSTATE (+9(09)), then
     * STRING prefix WS-SQLCODE ', STATE: ' WS-SQLSTATE DELIMITED BY SIZE INTO WS-FRD-ACT-MSG.
     */
    static String db2ErrorMessage(String currentMsg, String prefix, Sqlca sqlca) {
        String wsSqlcode = Pic.plusEdited(sqlca.sqlcode(), 6);
        String wsSqlstate = Pic.plusEditedFromAlphanumeric(sqlca.sqlstate(), 9);
        return Pic.stringInto(Pic.x(currentMsg, FraudStatusRecord.MSG_LENGTH), prefix, wsSqlcode, STATE_LABEL,
                wsSqlstate);
    }
}
