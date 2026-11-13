package com.carddemo.data.entity;

import jakarta.persistence.Column;
import jakarta.persistence.EmbeddedId;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

import java.math.BigDecimal;

/**
 * Pending authorization detail (IMS child segment PAUTDTL1 of DBPAUTP0).
 * <p>Source copybook: CIPAUDTY (optional IMS/DB2/MQ module).
 */
@Entity
@Table(name = "pending_auth_detail")
public class PendingAuthorizationDetail {

    @EmbeddedId
    private PendingAuthorizationDetailId id;

    /** PA-AUTH-ORIG-DATE PIC X(06) */
    @Column(name = "auth_orig_date", length = 6)
    private String authOrigDate;

    /** PA-AUTH-ORIG-TIME PIC X(06) */
    @Column(name = "auth_orig_time", length = 6)
    private String authOrigTime;

    /** PA-CARD-NUM PIC X(16) */
    @Column(name = "card_num", length = 16)
    private String cardNumber;

    /** PA-AUTH-TYPE PIC X(04) */
    @Column(name = "auth_type", length = 4)
    private String authType;

    /** PA-CARD-EXPIRY-DATE PIC X(04) */
    @Column(name = "card_expiry_date", length = 4)
    private String cardExpiryDate;

    /** PA-MESSAGE-TYPE PIC X(06) */
    @Column(name = "message_type", length = 6)
    private String messageType;

    /** PA-MESSAGE-SOURCE PIC X(06) */
    @Column(name = "message_source", length = 6)
    private String messageSource;

    /** PA-AUTH-ID-CODE PIC X(06) */
    @Column(name = "auth_id_code", length = 6)
    private String authIdCode;

    /** PA-AUTH-RESP-CODE PIC X(02) ('00' = approved) */
    @Column(name = "auth_resp_code", length = 2)
    private String authRespCode;

    /** PA-AUTH-RESP-REASON PIC X(04) */
    @Column(name = "auth_resp_reason", length = 4)
    private String authRespReason;

    /** PA-PROCESSING-CODE PIC 9(06) */
    @Column(name = "processing_code")
    private Integer processingCode;

    /** PA-TRANSACTION-AMT PIC S9(10)V99 COMP-3 */
    @Column(name = "transaction_amt", precision = 12, scale = 2)
    private BigDecimal transactionAmount;

    /** PA-APPROVED-AMT PIC S9(10)V99 COMP-3 */
    @Column(name = "approved_amt", precision = 12, scale = 2)
    private BigDecimal approvedAmount;

    /** PA-MERCHANT-CATAGORY-CODE PIC X(04) */
    @Column(name = "merchant_category_code", length = 4)
    private String merchantCategoryCode;

    /** PA-ACQR-COUNTRY-CODE PIC X(03) */
    @Column(name = "acqr_country_code", length = 3)
    private String acquirerCountryCode;

    /** PA-POS-ENTRY-MODE PIC 9(02) */
    @Column(name = "pos_entry_mode")
    private Integer posEntryMode;

    /** PA-MERCHANT-ID PIC X(15) */
    @Column(name = "merchant_id", length = 15)
    private String merchantId;

    /** PA-MERCHANT-NAME PIC X(22) */
    @Column(name = "merchant_name", length = 22)
    private String merchantName;

    /** PA-MERCHANT-CITY PIC X(13) */
    @Column(name = "merchant_city", length = 13)
    private String merchantCity;

    /** PA-MERCHANT-STATE PIC X(02) */
    @Column(name = "merchant_state", length = 2)
    private String merchantState;

    /** PA-MERCHANT-ZIP PIC X(09) */
    @Column(name = "merchant_zip", length = 9)
    private String merchantZip;

    /** PA-TRANSACTION-ID PIC X(15) */
    @Column(name = "transaction_id", length = 15)
    private String transactionId;

    /** PA-MATCH-STATUS PIC X(01): P pending, D declined, E expired, M matched */
    @Column(name = "match_status", length = 1)
    private String matchStatus;

    /** PA-AUTH-FRAUD PIC X(01): F confirmed, R removed */
    @Column(name = "auth_fraud", length = 1)
    private String authFraud;

    /** PA-FRAUD-RPT-DATE PIC X(08) */
    @Column(name = "fraud_rpt_date", length = 8)
    private String fraudReportDate;

    public PendingAuthorizationDetailId getId() {
        return id;
    }

    public void setId(PendingAuthorizationDetailId id) {
        this.id = id;
    }

    public String getAuthOrigDate() {
        return authOrigDate;
    }

    public void setAuthOrigDate(String authOrigDate) {
        this.authOrigDate = authOrigDate;
    }

    public String getAuthOrigTime() {
        return authOrigTime;
    }

    public void setAuthOrigTime(String authOrigTime) {
        this.authOrigTime = authOrigTime;
    }

    public String getCardNumber() {
        return cardNumber;
    }

    public void setCardNumber(String cardNumber) {
        this.cardNumber = cardNumber;
    }

    public String getAuthType() {
        return authType;
    }

    public void setAuthType(String authType) {
        this.authType = authType;
    }

    public String getCardExpiryDate() {
        return cardExpiryDate;
    }

    public void setCardExpiryDate(String cardExpiryDate) {
        this.cardExpiryDate = cardExpiryDate;
    }

    public String getMessageType() {
        return messageType;
    }

    public void setMessageType(String messageType) {
        this.messageType = messageType;
    }

    public String getMessageSource() {
        return messageSource;
    }

    public void setMessageSource(String messageSource) {
        this.messageSource = messageSource;
    }

    public String getAuthIdCode() {
        return authIdCode;
    }

    public void setAuthIdCode(String authIdCode) {
        this.authIdCode = authIdCode;
    }

    public String getAuthRespCode() {
        return authRespCode;
    }

    public void setAuthRespCode(String authRespCode) {
        this.authRespCode = authRespCode;
    }

    public String getAuthRespReason() {
        return authRespReason;
    }

    public void setAuthRespReason(String authRespReason) {
        this.authRespReason = authRespReason;
    }

    public Integer getProcessingCode() {
        return processingCode;
    }

    public void setProcessingCode(Integer processingCode) {
        this.processingCode = processingCode;
    }

    public BigDecimal getTransactionAmount() {
        return transactionAmount;
    }

    public void setTransactionAmount(BigDecimal transactionAmount) {
        this.transactionAmount = transactionAmount;
    }

    public BigDecimal getApprovedAmount() {
        return approvedAmount;
    }

    public void setApprovedAmount(BigDecimal approvedAmount) {
        this.approvedAmount = approvedAmount;
    }

    public String getMerchantCategoryCode() {
        return merchantCategoryCode;
    }

    public void setMerchantCategoryCode(String merchantCategoryCode) {
        this.merchantCategoryCode = merchantCategoryCode;
    }

    public String getAcquirerCountryCode() {
        return acquirerCountryCode;
    }

    public void setAcquirerCountryCode(String acquirerCountryCode) {
        this.acquirerCountryCode = acquirerCountryCode;
    }

    public Integer getPosEntryMode() {
        return posEntryMode;
    }

    public void setPosEntryMode(Integer posEntryMode) {
        this.posEntryMode = posEntryMode;
    }

    public String getMerchantId() {
        return merchantId;
    }

    public void setMerchantId(String merchantId) {
        this.merchantId = merchantId;
    }

    public String getMerchantName() {
        return merchantName;
    }

    public void setMerchantName(String merchantName) {
        this.merchantName = merchantName;
    }

    public String getMerchantCity() {
        return merchantCity;
    }

    public void setMerchantCity(String merchantCity) {
        this.merchantCity = merchantCity;
    }

    public String getMerchantState() {
        return merchantState;
    }

    public void setMerchantState(String merchantState) {
        this.merchantState = merchantState;
    }

    public String getMerchantZip() {
        return merchantZip;
    }

    public void setMerchantZip(String merchantZip) {
        this.merchantZip = merchantZip;
    }

    public String getTransactionId() {
        return transactionId;
    }

    public void setTransactionId(String transactionId) {
        this.transactionId = transactionId;
    }

    public String getMatchStatus() {
        return matchStatus;
    }

    public void setMatchStatus(String matchStatus) {
        this.matchStatus = matchStatus;
    }

    public String getAuthFraud() {
        return authFraud;
    }

    public void setAuthFraud(String authFraud) {
        this.authFraud = authFraud;
    }

    public String getFraudReportDate() {
        return fraudReportDate;
    }

    public void setFraudReportDate(String fraudReportDate) {
        this.fraudReportDate = fraudReportDate;
    }
}
