package com.carddemo.programs.coactvwc.copybook;

/** CVCRD01Y: 88-levels of {@code CCARD-AID PIC X(5)}. */
public enum CcardAid {
    ENTER("ENTER"), CLEAR("CLEAR"), PA1("PA1  "), PA2("PA2  "),
    PFK01("PFK01"), PFK02("PFK02"), PFK03("PFK03"), PFK04("PFK04"), PFK05("PFK05"), PFK06("PFK06"),
    PFK07("PFK07"), PFK08("PFK08"), PFK09("PFK09"), PFK10("PFK10"), PFK11("PFK11"), PFK12("PFK12");

    private final String value;

    CcardAid(String value) {
        this.value = value;
    }

    public String value() {
        return value;
    }
}
