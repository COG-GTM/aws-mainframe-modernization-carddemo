package com.carddemo.programs.copaus2c.copybook;

import java.util.Arrays;
import java.util.Optional;

/**
 * COPAUS2C LINKAGE SECTION DFHCOMMAREA (272 bytes), passed by EXEC CICS LINK from COPAUS1C.
 */
public record FraudCommarea(
        long acctId,                          // WS-ACCT-ID     PIC 9(11)
        long custId,                          // WS-CUST-ID     PIC 9(9)
        PendingAuthDetails fraudAuthRecord,   // WS-FRAUD-AUTH-RECORD   COPY CIPAUDTY
        FraudStatusRecord fraudStatusRecord) { // WS-FRAUD-STATUS-RECORD

    public static final int LENGTH = 11 + 9 + PendingAuthDetails.LENGTH + FraudStatusRecord.LENGTH;

    public FraudCommarea {
        acctId = Pic.unsigned(acctId, 11);
        custId = Pic.unsigned(custId, 9);
    }

    /** WS-FRAUD-STATUS-RECORD. */
    public record FraudStatusRecord(
            String frdAction,         // WS-FRD-ACTION          PIC X(01)
            String frdUpdateStatus,   // WS-FRD-UPDATE-STATUS   PIC X(01)
            String frdActMsg) {       // WS-FRD-ACT-MSG         PIC X(50)

        public static final int LENGTH = 52;
        public static final int MSG_LENGTH = 50;

        public FraudStatusRecord {
            frdAction = Pic.x(frdAction, 1);
            frdUpdateStatus = Pic.x(frdUpdateStatus, 1);
            frdActMsg = Pic.x(frdActMsg, MSG_LENGTH);
        }

        /** 88 WS-REPORT-FRAUD VALUE 'F'. */
        public boolean isReportFraud() {
            return FraudAction.WS_REPORT_FRAUD.value.equals(frdAction);
        }

        /** 88 WS-REMOVE-FRAUD VALUE 'R'. */
        public boolean isRemoveFraud() {
            return FraudAction.WS_REMOVE_FRAUD.value.equals(frdAction);
        }

        /** 88 WS-FRD-UPDT-SUCCESS VALUE 'S'. */
        public boolean isUpdtSuccess() {
            return FraudUpdateStatus.WS_FRD_UPDT_SUCCESS.value.equals(frdUpdateStatus);
        }

        /** 88 WS-FRD-UPDT-FAILED VALUE 'F'. */
        public boolean isUpdtFailed() {
            return FraudUpdateStatus.WS_FRD_UPDT_FAILED.value.equals(frdUpdateStatus);
        }
    }

    /** WS-FRD-ACTION 88-levels. */
    public enum FraudAction {
        WS_REPORT_FRAUD("F"),
        WS_REMOVE_FRAUD("R");

        public final String value;

        FraudAction(String value) {
            this.value = value;
        }

        public static Optional<FraudAction> of(String v) {
            return Arrays.stream(values()).filter(a -> a.value.equals(v)).findFirst();
        }
    }

    /** WS-FRD-UPDATE-STATUS 88-levels. */
    public enum FraudUpdateStatus {
        WS_FRD_UPDT_SUCCESS("S"),
        WS_FRD_UPDT_FAILED("F");

        public final String value;

        FraudUpdateStatus(String value) {
            this.value = value;
        }

        public static Optional<FraudUpdateStatus> of(String v) {
            return Arrays.stream(values()).filter(a -> a.value.equals(v)).findFirst();
        }
    }

    public static FraudCommarea fromEbcdic(byte[] rec) {
        return new FraudCommarea(
                Long.parseLong(Ebcdic.text(rec, 0, 11)),
                Long.parseLong(Ebcdic.text(rec, 11, 9)),
                PendingAuthDetails.fromEbcdic(rec, 20),
                new FraudStatusRecord(Ebcdic.text(rec, 220, 1), Ebcdic.text(rec, 221, 1), Ebcdic.text(rec, 222, 50)));
    }

    public byte[] toEbcdic() {
        byte[] rec = new byte[LENGTH];
        Ebcdic.putText(rec, 0, 11, Pic.display9(acctId, 11));
        Ebcdic.putText(rec, 11, 9, Pic.display9(custId, 9));
        fraudAuthRecord.toEbcdic(rec, 20);
        Ebcdic.putText(rec, 220, 1, fraudStatusRecord.frdAction());
        Ebcdic.putText(rec, 221, 1, fraudStatusRecord.frdUpdateStatus());
        Ebcdic.putText(rec, 222, 50, fraudStatusRecord.frdActMsg());
        return rec;
    }
}
