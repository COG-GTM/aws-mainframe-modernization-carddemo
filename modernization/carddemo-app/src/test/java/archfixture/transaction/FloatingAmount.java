package archfixture.transaction;

import java.math.BigDecimal;

/** Illegal: binary floating point for money. */
public class FloatingAmount {
    public double amount;
    public Float rate;
    public BigDecimal exact;
}
