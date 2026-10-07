package com.carddemo.web.card;

import com.carddemo.web.security.JwtProperties;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.security.GeneralSecurityException;
import java.security.MessageDigest;
import java.util.Arrays;
import java.util.Base64;
import java.util.Optional;
import java.util.regex.Pattern;
import javax.crypto.Cipher;
import javax.crypto.Mac;
import javax.crypto.spec.GCMParameterSpec;
import javax.crypto.spec.SecretKeySpec;
import org.springframework.boot.autoconfigure.condition.ConditionalOnWebApplication;
import org.springframework.stereotype.Component;

/**
 * Opaque card references (ADR-0020): the list hands out {@code cardRef} instead of the full PAN, and every card
 * endpoint accepts either. A reference is the card number encrypted with AES-256-GCM under a key derived from
 * {@code CARDDEMO_JWT_SECRET}; the nonce is derived from the card number (HMAC-SHA256), so a card always gets the
 * same reference and tampered references fail authentication.
 */
@Component
@ConditionalOnWebApplication(type = ConditionalOnWebApplication.Type.SERVLET)
public class CardReferences {

    private static final Pattern DIGITS = Pattern.compile("\\d{1,16}");
    private static final Pattern PAN = Pattern.compile("\\d{16}");
    private static final byte[] AAD = "carddemo-card-ref-v1".getBytes(StandardCharsets.US_ASCII);
    private static final int NONCE_BYTES = 12;
    private static final int TAG_BITS = 128;

    private final SecretKeySpec encryptionKey;
    private final SecretKeySpec nonceKey;

    public CardReferences(JwtProperties jwt) {
        this.encryptionKey = new SecretKeySpec(derive("card-ref-enc", jwt.secret()), "AES");
        this.nonceKey = new SecretKeySpec(derive("card-ref-nonce", jwt.secret()), "HmacSHA256");
    }

    public String encode(String cardNum) {
        try {
            Mac mac = Mac.getInstance("HmacSHA256");
            mac.init(nonceKey);
            byte[] nonce = Arrays.copyOf(mac.doFinal(cardNum.getBytes(StandardCharsets.US_ASCII)), NONCE_BYTES);
            Cipher cipher = Cipher.getInstance("AES/GCM/NoPadding");
            cipher.init(Cipher.ENCRYPT_MODE, encryptionKey, new GCMParameterSpec(TAG_BITS, nonce));
            cipher.updateAAD(AAD);
            byte[] sealed = cipher.doFinal(cardNum.getBytes(StandardCharsets.US_ASCII));
            return Base64.getUrlEncoder().withoutPadding()
                    .encodeToString(ByteBuffer.allocate(nonce.length + sealed.length).put(nonce).put(sealed).array());
        } catch (GeneralSecurityException e) {
            throw new IllegalStateException("card reference encryption failed", e);
        }
    }

    /** The card number of a reference this application issued; empty for anything else. */
    public Optional<String> decode(String ref) {
        try {
            byte[] raw = Base64.getUrlDecoder().decode(ref);
            if (raw.length <= NONCE_BYTES) {
                return Optional.empty();
            }
            Cipher cipher = Cipher.getInstance("AES/GCM/NoPadding");
            cipher.init(Cipher.DECRYPT_MODE, encryptionKey,
                    new GCMParameterSpec(TAG_BITS, Arrays.copyOf(raw, NONCE_BYTES)));
            cipher.updateAAD(AAD);
            String cardNum = new String(cipher.doFinal(raw, NONCE_BYTES, raw.length - NONCE_BYTES),
                    StandardCharsets.US_ASCII);
            return PAN.matcher(cardNum).matches() ? Optional.of(cardNum) : Optional.empty();
        } catch (GeneralSecurityException | IllegalArgumentException e) {
            return Optional.empty();
        }
    }

    /**
     * A card number as typed (digits, edited later like {@code CARDSIDI}) or a {@code cardRef}; any other value is
     * returned unchanged so the COBOL edit rejects it with its own message.
     */
    public String cardNumberOrRef(String value) {
        if (value == null || DIGITS.matcher(value.strip()).matches()) {
            return value;
        }
        return decode(value.strip()).orElse(value);
    }

    /** A browse cursor: a 1-16 digit card number (zero-padded) or a {@code cardRef}; empty when it is neither. */
    public Optional<String> cursor(String value) {
        String v = value.strip();
        if (DIGITS.matcher(v).matches()) {
            return Optional.of(String.format("%016d", Long.parseLong(v)));
        }
        return decode(v);
    }

    private static byte[] derive(String purpose, String secret) {
        try {
            return MessageDigest.getInstance("SHA-256")
                    .digest((purpose + ":" + secret).getBytes(StandardCharsets.UTF_8));
        } catch (GeneralSecurityException e) {
            throw new IllegalStateException(e);
        }
    }
}
