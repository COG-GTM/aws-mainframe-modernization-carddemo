package com.carddemo.programs.cotrn02c;

/**
 * Inbound DTO for map COTRN2A (the unprotected input fields of COTRN2AI). A {@code null} value means the
 * field was not transmitted (arrives as LOW-VALUES); any other value is space-padded to the field length.
 */
public record Cotrn2aRequest(
        String acctId,        // ACTIDINI X(11)
        String cardNum,       // CARDNINI X(16)
        String typeCd,        // TTYPCDI  X(2)
        String catCd,         // TCATCDI  X(4)
        String source,        // TRNSRCI  X(10)
        String desc,          // TDESCI   X(60)
        String amount,        // TRNAMTI  X(12)
        String origDate,      // TORIGDTI X(10)
        String procDate,      // TPROCDTI X(10)
        String merchantId,    // MIDI     X(9)
        String merchantName,  // MNAMEI   X(30)
        String merchantCity,  // MCITYI   X(25)
        String merchantZip,   // MZIPI    X(10)
        String confirm) {     // CONFIRMI X(1)

    public static final Cotrn2aRequest EMPTY = builder().build();

    public static Builder builder() {
        return new Builder();
    }

    public Builder toBuilder() {
        return new Builder().acctId(acctId).cardNum(cardNum).typeCd(typeCd).catCd(catCd).source(source)
                .desc(desc).amount(amount).origDate(origDate).procDate(procDate).merchantId(merchantId)
                .merchantName(merchantName).merchantCity(merchantCity).merchantZip(merchantZip).confirm(confirm);
    }

    public static final class Builder {
        private String acctId;
        private String cardNum;
        private String typeCd;
        private String catCd;
        private String source;
        private String desc;
        private String amount;
        private String origDate;
        private String procDate;
        private String merchantId;
        private String merchantName;
        private String merchantCity;
        private String merchantZip;
        private String confirm;

        public Builder acctId(String v) {
            acctId = v;
            return this;
        }

        public Builder cardNum(String v) {
            cardNum = v;
            return this;
        }

        public Builder typeCd(String v) {
            typeCd = v;
            return this;
        }

        public Builder catCd(String v) {
            catCd = v;
            return this;
        }

        public Builder source(String v) {
            source = v;
            return this;
        }

        public Builder desc(String v) {
            desc = v;
            return this;
        }

        public Builder amount(String v) {
            amount = v;
            return this;
        }

        public Builder origDate(String v) {
            origDate = v;
            return this;
        }

        public Builder procDate(String v) {
            procDate = v;
            return this;
        }

        public Builder merchantId(String v) {
            merchantId = v;
            return this;
        }

        public Builder merchantName(String v) {
            merchantName = v;
            return this;
        }

        public Builder merchantCity(String v) {
            merchantCity = v;
            return this;
        }

        public Builder merchantZip(String v) {
            merchantZip = v;
            return this;
        }

        public Builder confirm(String v) {
            confirm = v;
            return this;
        }

        public Cotrn2aRequest build() {
            return new Cotrn2aRequest(acctId, cardNum, typeCd, catCd, source, desc, amount, origDate, procDate,
                    merchantId, merchantName, merchantCity, merchantZip, confirm);
        }
    }
}
