package com.carddemo.programs.coactvwc.copybook;

import com.carddemo.programs.coactvwc.support.Cobol;

/**
 * COACTVW (cpy-bms) symbolic map {@code CACTVWAO}: what SEND MAP puts on the screen. Values are stored at their
 * exact PIC width; {@code null} means LOW-VALUES (field not sent, screen default kept).
 */
public final class CactvwaOutput {
    // MOVE LOW-VALUES TO CACTVWAO: every field starts null.
    private String trnnameo, title01o, curdateo, pgmnameo, title02o, curtimeo;
    private String acctsido, acsttuso, adtopeno, acrdlimo, aexpdto, acshlimo, areisdto, acurbalo, acrcycro;
    private String aaddgrpo, acrcydbo, acstnumo, acstssno, acstdobo, acstfcoo, acsfnamo, acsmnamo, acslnamo;
    private String acsadl1o, acsstteo, acsadl2o, acszipco, acscityo, acsctryo, acsphn1o, acsgovto, acsphn2o;
    private String acseftco, acspflgo, infomsgo, errmsgo;

    /** ACCTSIDA (attribute byte), ACCTSIDC (colour), INFOMSGC (colour); LOW-VALUE when unset. */
    private char acctsida = Cobol.LOW_VALUE;
    private char acctsidc = Cobol.LOW_VALUE;
    private char infomsgc = Cobol.LOW_VALUE;
    /** ACCTSIDL = -1: symbolic cursor positioning (SEND MAP ... CURSOR). */
    private int acctsidl;

    public String trnnameo() { return trnnameo; }
    public void setTrnnameo(String v) { trnnameo = Cobol.alnum(v, 4); }
    public String title01o() { return title01o; }
    public void setTitle01o(String v) { title01o = Cobol.alnum(v, 40); }
    public String curdateo() { return curdateo; }
    public void setCurdateo(String v) { curdateo = Cobol.alnum(v, 8); }
    public String pgmnameo() { return pgmnameo; }
    public void setPgmnameo(String v) { pgmnameo = Cobol.alnum(v, 8); }
    public String title02o() { return title02o; }
    public void setTitle02o(String v) { title02o = Cobol.alnum(v, 40); }
    public String curtimeo() { return curtimeo; }
    public void setCurtimeo(String v) { curtimeo = Cobol.alnum(v, 8); }
    /** ACCTSIDO PIC X(11). */
    public String acctsido() { return acctsido; }
    public void setAcctsido(String v) { acctsido = v == null ? null : Cobol.alnum(v, 11); }
    public String acsttuso() { return acsttuso; }
    public void setAcsttuso(String v) { acsttuso = Cobol.alnum(v, 1); }
    public String adtopeno() { return adtopeno; }
    public void setAdtopeno(String v) { adtopeno = Cobol.alnum(v, 10); }
    /** ACRDLIMO PIC +ZZZ,ZZZ,ZZZ.99. */
    public String acrdlimo() { return acrdlimo; }
    public void setAcrdlimo(String v) { acrdlimo = Cobol.alnum(v, 15); }
    public String aexpdto() { return aexpdto; }
    public void setAexpdto(String v) { aexpdto = Cobol.alnum(v, 10); }
    /** ACSHLIMO PIC +ZZZ,ZZZ,ZZZ.99. */
    public String acshlimo() { return acshlimo; }
    public void setAcshlimo(String v) { acshlimo = Cobol.alnum(v, 15); }
    public String areisdto() { return areisdto; }
    public void setAreisdto(String v) { areisdto = Cobol.alnum(v, 10); }
    /** ACURBALO PIC +ZZZ,ZZZ,ZZZ.99. */
    public String acurbalo() { return acurbalo; }
    public void setAcurbalo(String v) { acurbalo = Cobol.alnum(v, 15); }
    /** ACRCYCRO PIC +ZZZ,ZZZ,ZZZ.99. */
    public String acrcycro() { return acrcycro; }
    public void setAcrcycro(String v) { acrcycro = Cobol.alnum(v, 15); }
    public String aaddgrpo() { return aaddgrpo; }
    public void setAaddgrpo(String v) { aaddgrpo = Cobol.alnum(v, 10); }
    /** ACRCYDBO PIC +ZZZ,ZZZ,ZZZ.99. */
    public String acrcydbo() { return acrcydbo; }
    public void setAcrcydbo(String v) { acrcydbo = Cobol.alnum(v, 15); }
    public String acstnumo() { return acstnumo; }
    public void setAcstnumo(String v) { acstnumo = Cobol.alnum(v, 9); }
    /**
     * ACSTSSNO PIC X(12), filled by STRING (11 bytes); the 12th byte keeps its LOW-VALUE from MOVE LOW-VALUES.
     */
    public String acstssno() { return acstssno; }
    public void setAcstssno(String v) { acstssno = v; }
    public String acstdobo() { return acstdobo; }
    public void setAcstdobo(String v) { acstdobo = Cobol.alnum(v, 10); }
    public String acstfcoo() { return acstfcoo; }
    public void setAcstfcoo(String v) { acstfcoo = Cobol.alnum(v, 3); }
    public String acsfnamo() { return acsfnamo; }
    public void setAcsfnamo(String v) { acsfnamo = Cobol.alnum(v, 25); }
    public String acsmnamo() { return acsmnamo; }
    public void setAcsmnamo(String v) { acsmnamo = Cobol.alnum(v, 25); }
    public String acslnamo() { return acslnamo; }
    public void setAcslnamo(String v) { acslnamo = Cobol.alnum(v, 25); }
    public String acsadl1o() { return acsadl1o; }
    public void setAcsadl1o(String v) { acsadl1o = Cobol.alnum(v, 50); }
    public String acsstteo() { return acsstteo; }
    public void setAcsstteo(String v) { acsstteo = Cobol.alnum(v, 2); }
    public String acsadl2o() { return acsadl2o; }
    public void setAcsadl2o(String v) { acsadl2o = Cobol.alnum(v, 50); }
    /** ACSZIPCO PIC X(5): CUST-ADDR-ZIP X(10) is truncated. */
    public String acszipco() { return acszipco; }
    public void setAcszipco(String v) { acszipco = Cobol.alnum(v, 5); }
    public String acscityo() { return acscityo; }
    public void setAcscityo(String v) { acscityo = Cobol.alnum(v, 50); }
    public String acsctryo() { return acsctryo; }
    public void setAcsctryo(String v) { acsctryo = Cobol.alnum(v, 3); }
    /** ACSPHN1O PIC X(13): CUST-PHONE-NUM-1 X(15) is truncated. */
    public String acsphn1o() { return acsphn1o; }
    public void setAcsphn1o(String v) { acsphn1o = Cobol.alnum(v, 13); }
    public String acsgovto() { return acsgovto; }
    public void setAcsgovto(String v) { acsgovto = Cobol.alnum(v, 20); }
    public String acsphn2o() { return acsphn2o; }
    public void setAcsphn2o(String v) { acsphn2o = Cobol.alnum(v, 13); }
    public String acseftco() { return acseftco; }
    public void setAcseftco(String v) { acseftco = Cobol.alnum(v, 10); }
    public String acspflgo() { return acspflgo; }
    public void setAcspflgo(String v) { acspflgo = Cobol.alnum(v, 1); }
    /** INFOMSGO PIC X(45). */
    public String infomsgo() { return infomsgo; }
    public void setInfomsgo(String v) { infomsgo = Cobol.alnum(v, 45); }
    /** ERRMSGO PIC X(78). */
    public String errmsgo() { return errmsgo; }
    public void setErrmsgo(String v) { errmsgo = Cobol.alnum(v, 78); }

    public char acctsida() { return acctsida; }
    public void setAcctsida(char v) { acctsida = v; }
    public char acctsidc() { return acctsidc; }
    public void setAcctsidc(char v) { acctsidc = v; }
    public char infomsgc() { return infomsgc; }
    public void setInfomsgc(char v) { infomsgc = v; }
    public int acctsidl() { return acctsidl; }
    public void setAcctsidl(int v) { acctsidl = v; }
    /** True when ACCTSIDL = -1, i.e. the cursor is placed on the account-id field. */
    public boolean cursorOnAcctsid() { return acctsidl == -1; }
}
