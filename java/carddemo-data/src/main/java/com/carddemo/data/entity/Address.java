package com.carddemo.data.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Embeddable;

/** Postal address carried as six consecutive CUST-ADDR-* items of CVCUS01Y, grouped as an embeddable. */
@Embeddable
public class Address {

    /** CUST-ADDR-LINE-1 PIC X(50) */
    @Column(name = "addr_line_1", length = 50)
    private String line1;

    /** CUST-ADDR-LINE-2 PIC X(50) */
    @Column(name = "addr_line_2", length = 50)
    private String line2;

    /** CUST-ADDR-LINE-3 PIC X(50) */
    @Column(name = "addr_line_3", length = 50)
    private String line3;

    /** CUST-ADDR-STATE-CD PIC X(02) */
    @Column(name = "addr_state_cd", length = 2)
    private String stateCode;

    /** CUST-ADDR-COUNTRY-CD PIC X(03) */
    @Column(name = "addr_country_cd", length = 3)
    private String countryCode;

    /** CUST-ADDR-ZIP PIC X(10) */
    @Column(name = "addr_zip", length = 10)
    private String zip;

    public String getLine1() {
        return line1;
    }

    public void setLine1(String line1) {
        this.line1 = line1;
    }

    public String getLine2() {
        return line2;
    }

    public void setLine2(String line2) {
        this.line2 = line2;
    }

    public String getLine3() {
        return line3;
    }

    public void setLine3(String line3) {
        this.line3 = line3;
    }

    public String getStateCode() {
        return stateCode;
    }

    public void setStateCode(String stateCode) {
        this.stateCode = stateCode;
    }

    public String getCountryCode() {
        return countryCode;
    }

    public void setCountryCode(String countryCode) {
        this.countryCode = countryCode;
    }

    public String getZip() {
        return zip;
    }

    public void setZip(String zip) {
        this.zip = zip;
    }
}
