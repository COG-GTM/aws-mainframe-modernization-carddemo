package com.carddemo.zerotrust.policy;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/**
 * Reads the user security file ({@code AWS.M2.CARDDEMO.USRSEC.PS}), 80-byte EBCDIC
 * records in the CSUSR01Y layout:
 * <pre>
 *   SEC-USR-ID    X(08)  @0
 *   SEC-USR-FNAME X(20)  @8
 *   SEC-USR-LNAME X(20)  @28
 *   SEC-USR-PWD   X(08)  @48   (never decoded — see below)
 *   SEC-USR-TYPE  X(01)  @56
 *   FILLER        X(23)  @57
 * </pre>
 * Only the id and type bytes are decoded. The password field is never read or
 * retained: human credentials move to the platform identity provider, and this
 * parser exists solely to derive subject attributes for authorization.
 */
public final class UserSecurityFile {

    public static final int RECORD_LENGTH = 80;
    private static final int ID_OFFSET = 0;
    private static final int ID_LENGTH = 8;
    private static final int TYPE_OFFSET = 56;
    private static final Charset EBCDIC = Charset.forName("IBM037");

    private UserSecurityFile() {
    }

    public static List<CardDemoUser> load(Path path) {
        byte[] bytes;
        try {
            bytes = Files.readAllBytes(path);
        } catch (IOException e) {
            throw new UncheckedIOException("cannot read user security file " + path, e);
        }
        if (bytes.length % RECORD_LENGTH != 0) {
            throw new IllegalArgumentException(
                    "user security file length " + bytes.length + " is not a multiple of " + RECORD_LENGTH);
        }
        List<CardDemoUser> users = new ArrayList<>();
        for (int off = 0; off < bytes.length; off += RECORD_LENGTH) {
            String id = new String(bytes, off + ID_OFFSET, ID_LENGTH, EBCDIC).trim();
            String type = new String(bytes, off + TYPE_OFFSET, 1, EBCDIC).trim();
            users.add(new CardDemoUser(id, type));
        }
        return users;
    }
}
