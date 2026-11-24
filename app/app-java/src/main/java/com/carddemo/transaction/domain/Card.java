package com.carddemo.transaction.domain;

import com.carddemo.transaction.cobol.Pic;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;

/**
 * CARD-RECORD (copybook CVACT02Y, RECLN 150) - CARDDATA VSAM KSDS keyed by CARD-NUM.
 */
@Entity
@Table(name = "CARDDATA")
public class Card {

    @Id
    @Column(name = "CARD_NUM", length = 16)
    private String cardNum;                 // PIC X(16)
    @Column(name = "CARD_ACCT_ID")
    private Long acctId;                    // PIC 9(11)
    @Column(name = "CARD_CVV_CD")
    private Integer cvvCd;                  // PIC 9(03)
    @Column(name = "CARD_EMBOSSED_NAME", length = 50)
    private String embossedName;            // PIC X(50)
    @Column(name = "CARD_EXPIRAION_DATE", length = 10)
    private String expirationDate;          // PIC X(10)
    @Column(name = "CARD_ACTIVE_STATUS", length = 1)
    private String activeStatus;            // PIC X(01)

    public String getCardNum() { return cardNum; }
    public void setCardNum(String v) { this.cardNum = Pic.x(v, 16); }
    public Long getAcctId() { return acctId; }
    public void setAcctId(long v) { this.acctId = Pic.n9(v, 11); }
    public Integer getCvvCd() { return cvvCd; }
    public void setCvvCd(int v) { this.cvvCd = Pic.n9(v, 3); }
    public String getEmbossedName() { return embossedName; }
    public void setEmbossedName(String v) { this.embossedName = Pic.x(v, 50); }
    public String getExpirationDate() { return expirationDate; }
    public void setExpirationDate(String v) { this.expirationDate = Pic.x(v, 10); }
    public String getActiveStatus() { return activeStatus; }
    public void setActiveStatus(String v) { this.activeStatus = Pic.x(v, 1); }
}
