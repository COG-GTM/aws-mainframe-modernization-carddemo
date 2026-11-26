package com.carddemo.programs.cocrdupc;

import static com.carddemo.programs.cocrdupc.Cobol.alnum;
import static com.carddemo.programs.cocrdupc.Cobol.displayValue;
import static com.carddemo.programs.cocrdupc.Cobol.lowValues;
import static com.carddemo.programs.cocrdupc.Cobol.numeric;
import static com.carddemo.programs.cocrdupc.Cobol.spaces;

import com.carddemo.programs.cocrdupc.copybook.CardRecord;

/**
 * 01 WS-THIS-PROGCOMMAREA (329 bytes): the program-private part of the COMMAREA that follows
 * CARDDEMO-COMMAREA.
 */
public final class CardUpdateProgramArea {

    public static final int LENGTH = 329;

    /** CCUP-CHANGE-ACTION PIC X(1) values; SET ... TO TRUE moves the first listed value. */
    public enum ChangeAction {
        DETAILS_NOT_FETCHED(Cobol.LOW_VALUE),
        SHOW_DETAILS('S'),
        CHANGES_NOT_OK('E'),
        CHANGES_OK_NOT_CONFIRMED('N'),
        CHANGES_OKAYED_AND_DONE('C'),
        CHANGES_OKAYED_LOCK_ERROR('L'),
        CHANGES_OKAYED_BUT_FAILED('F');

        private final char value;

        ChangeAction(char value) {
            this.value = value;
        }

        public char value() {
            return value;
        }
    }

    /** CCUP-OLD-DETAILS / CCUP-NEW-DETAILS (89 bytes each). */
    public static final class CardDetails {
        private String acctId;   // X(11)
        private String cardId;   // X(16)
        private String cvvCd;    // X(3)
        // CCUP-xxx-CARDDATA (59 bytes)
        private String crdName;  // X(50)
        private String expYear;  // X(4)
        private String expMon;   // X(2)
        private String expDay;   // X(2)
        private String crdStCd;  // X(1)

        CardDetails(String fill) {
            acctId = fill.repeat(11);
            cardId = fill.repeat(16);
            cvvCd = fill.repeat(3);
            setCardData(fill.repeat(59));
        }

        /** INITIALIZE: every field is PIC X, so SPACES. */
        public void initialize() {
            acctId = spaces(11);
            cardId = spaces(16);
            cvvCd = spaces(3);
            setCardData(spaces(59));
        }

        String encode() {
            return acctId + cardId + cvvCd + cardData();
        }

        void decode(String r) {
            acctId = r.substring(0, 11);
            cardId = r.substring(11, 27);
            cvvCd = r.substring(27, 30);
            setCardData(r.substring(30, 89));
        }

        /** CCUP-xxx-CARDDATA group. */
        public String cardData() {
            return crdName + expYear + expMon + expDay + crdStCd;
        }

        public void setCardData(String v) {
            String r = alnum(v, 59);
            crdName = r.substring(0, 50);
            expYear = r.substring(50, 54);
            expMon = r.substring(54, 56);
            expDay = r.substring(56, 58);
            crdStCd = r.substring(58, 59);
        }

        public String acctId() { return acctId; }
        public void setAcctId(String v) { acctId = alnum(v, 11); }
        public String cardId() { return cardId; }
        public void setCardId(String v) { cardId = alnum(v, 16); }
        public String cvvCd() { return cvvCd; }
        public void setCvvCd(String v) { cvvCd = alnum(v, 3); }
        public String crdName() { return crdName; }
        public void setCrdName(String v) { crdName = alnum(v, 50); }
        public String expYear() { return expYear; }
        public void setExpYear(String v) { expYear = alnum(v, 4); }
        public String expMon() { return expMon; }
        public void setExpMon(String v) { expMon = alnum(v, 2); }
        public String expDay() { return expDay; }
        public void setExpDay(String v) { expDay = alnum(v, 2); }
        public String crdStCd() { return crdStCd; }
        public void setCrdStCd(String v) { crdStCd = alnum(v, 1); }
    }

    /** 05 CARD-UPDATE-RECORD: same 150-byte layout as CVACT02Y CARD-RECORD. */
    public static final class CardUpdateRecord {
        private String num;              // X(16)
        private long acctId;             // 9(11)
        private int cvvCd;               // 9(03)
        private String embossedName;     // X(50)
        private String expiraionDate;    // X(10)
        private String activeStatus;     // X(01)
        private String filler;           // X(59)

        CardUpdateRecord(String fill) {
            num = fill.repeat(16);
            embossedName = fill.repeat(50);
            expiraionDate = fill.repeat(10);
            activeStatus = fill;
            filler = fill.repeat(59);
        }

        /** INITIALIZE CARD-UPDATE-RECORD: FILLER is not touched. */
        public void initialize() {
            num = spaces(16);
            acctId = 0;
            cvvCd = 0;
            embossedName = spaces(50);
            expiraionDate = spaces(10);
            activeStatus = spaces(1);
        }

        String encode() {
            return num + numeric(acctId, 11) + numeric(cvvCd, 3) + embossedName + expiraionDate
                    + activeStatus + filler;
        }

        void decode(String r) {
            num = r.substring(0, 16);
            acctId = displayValue(r.substring(16, 27));
            cvvCd = (int) displayValue(r.substring(27, 30));
            embossedName = r.substring(30, 80);
            expiraionDate = r.substring(80, 90);
            activeStatus = r.substring(90, 91);
            filler = r.substring(91, 150);
        }

        public CardRecord toCardRecord() {
            return new CardRecord(num, acctId, cvvCd, embossedName, expiraionDate, activeStatus, filler);
        }

        public String num() { return num; }
        public void setNum(String v) { num = alnum(v, 16); }
        public long acctId() { return acctId; }
        public void setAcctId(long v) { acctId = displayValue(numeric(v, 11)); }
        public int cvvCd() { return cvvCd; }
        public void setCvvCd(long v) { cvvCd = (int) displayValue(numeric(v, 3)); }
        public String embossedName() { return embossedName; }
        public void setEmbossedName(String v) { embossedName = alnum(v, 50); }
        public String expiraionDate() { return expiraionDate; }
        public void setExpiraionDate(String v) { expiraionDate = alnum(v, 10); }
        public String activeStatus() { return activeStatus; }
        public void setActiveStatus(String v) { activeStatus = alnum(v, 1); }
        public String filler() { return filler; }
    }

    private char changeAction;
    private final CardDetails oldDetails;
    private final CardDetails newDetails;
    private final CardUpdateRecord cardUpdateRecord;

    private CardUpdateProgramArea(String fill) {
        changeAction = Cobol.LOW_VALUE;
        oldDetails = new CardDetails(fill);
        newDetails = new CardDetails(fill);
        cardUpdateRecord = new CardUpdateRecord(fill);
    }

    /** Fresh working storage: CCUP-CHANGE-ACTION VALUE LOW-VALUES, all else LOW-VALUES. */
    public static CardUpdateProgramArea uninitialized() {
        return new CardUpdateProgramArea(lowValues(1));
    }

    /** INITIALIZE WS-THIS-PROGCOMMAREA. */
    public void initialize() {
        changeAction = ' ';
        oldDetails.initialize();
        newDetails.initialize();
        cardUpdateRecord.initialize();
    }

    public static CardUpdateProgramArea decode(String raw) {
        String r = alnum(raw, LENGTH);
        CardUpdateProgramArea a = new CardUpdateProgramArea(" ");
        a.changeAction = r.charAt(0);
        a.oldDetails.decode(r.substring(1, 90));
        a.newDetails.decode(r.substring(90, 179));
        a.cardUpdateRecord.decode(r.substring(179, 329));
        return a;
    }

    public String encode() {
        return changeAction + oldDetails.encode() + newDetails.encode() + cardUpdateRecord.encode();
    }

    public char changeAction() { return changeAction; }
    public void setChangeAction(char v) { changeAction = v; }
    public void set(ChangeAction a) { changeAction = a.value(); }
    public CardDetails oldDetails() { return oldDetails; }
    public CardDetails newDetails() { return newDetails; }
    public CardUpdateRecord cardUpdateRecord() { return cardUpdateRecord; }

    /** 88 CCUP-DETAILS-NOT-FETCHED VALUES LOW-VALUES, SPACES. */
    public boolean isDetailsNotFetched() {
        return changeAction == Cobol.LOW_VALUE || changeAction == ' ';
    }

    public boolean isShowDetails() { return changeAction == 'S'; }

    /** 88 CCUP-CHANGES-MADE VALUES 'E','N','C','L','F'. */
    public boolean isChangesMade() { return "ENCLF".indexOf(changeAction) >= 0; }

    public boolean isChangesNotOk() { return changeAction == 'E'; }
    public boolean isChangesOkNotConfirmed() { return changeAction == 'N'; }
    public boolean isChangesOkayedAndDone() { return changeAction == 'C'; }

    /** 88 CCUP-CHANGES-FAILED VALUES 'L','F'. */
    public boolean isChangesFailed() { return changeAction == 'L' || changeAction == 'F'; }

    public boolean isChangesOkayedLockError() { return changeAction == 'L'; }
    public boolean isChangesOkayedButFailed() { return changeAction == 'F'; }
}
