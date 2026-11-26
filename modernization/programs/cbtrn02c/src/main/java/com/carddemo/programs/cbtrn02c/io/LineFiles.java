package com.carddemo.programs.cbtrn02c.io;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.attribute.PosixFileAttributeView;
import java.nio.file.attribute.PosixFileAttributes;
import java.nio.file.attribute.PosixFilePermission;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;

/** Line-per-record text files (the app/data/ASCII layout); CR is stripped, short lines are space padded. */
public final class LineFiles {
    private LineFiles() {
    }

    public static List<String> readRecords(Path path, int recordLength) {
        try {
            List<String> out = new ArrayList<>();
            for (String line : Files.readAllLines(path, StandardCharsets.ISO_8859_1)) {
                String l = line.endsWith("\r") ? line.substring(0, line.length() - 1) : line;
                if (l.isEmpty()) {
                    continue;
                }
                if (l.length() > recordLength) {
                    throw new IOException("record longer than LRECL " + recordLength + " in " + path);
                }
                out.add(fit(l, recordLength));
            }
            return out;
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    /**
     * Replaces the file atomically: the records go to a temp file next to the symlink-resolved target, which
     * takes the old file's permissions (owner-only for a new file) and is then moved over it, so a failed write
     * leaves the previous dataset intact. Non-regular targets such as /dev/null are written directly.
     */
    public static void writeRecords(Path path, List<String> records) {
        try {
            Path target = resolveLinks(path);
            boolean exists = Files.exists(target);
            if (exists && !Files.isRegularFile(target)) {
                Files.write(target, records, StandardCharsets.ISO_8859_1);
                return;
            }
            Path tmp = Files.createTempFile(target.getParent(), "." + target.getFileName(), ".tmp");
            try {
                if (tmp.getFileSystem().supportedFileAttributeViews().contains("posix")) {
                    Files.setPosixFilePermissions(tmp, exists
                            ? inheritedPermissions(tmp, target)
                            : PosixFilePermissions.fromString("rw-------"));
                }
                Files.write(tmp, records, StandardCharsets.ISO_8859_1);
                Files.move(tmp, target, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING);
            } finally {
                Files.deleteIfExists(tmp);
            }
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    /** Gives tmp the target's group; if that is not allowed, the group gets no access rather than the old group's. */
    private static Set<PosixFilePermission> inheritedPermissions(Path tmp, Path target) throws IOException {
        Set<PosixFilePermission> perms = EnumSet.noneOf(PosixFilePermission.class);
        perms.addAll(Files.getPosixFilePermissions(target));
        try {
            Files.getFileAttributeView(tmp, PosixFileAttributeView.class)
                    .setGroup(Files.readAttributes(target, PosixFileAttributes.class).group());
        } catch (IOException e) {
            perms.removeAll(EnumSet.of(PosixFilePermission.GROUP_READ, PosixFilePermission.GROUP_WRITE,
                    PosixFilePermission.GROUP_EXECUTE));
        }
        return perms;
    }

    /** Follows symlinks, including one whose target does not exist yet, so the link itself is never replaced. */
    private static Path resolveLinks(Path path) throws IOException {
        Path p = path.toAbsolutePath();
        for (int i = 0; i < 40 && Files.isSymbolicLink(p); i++) {
            p = p.resolveSibling(Files.readSymbolicLink(p));
        }
        if (Files.isSymbolicLink(p)) {
            throw new IOException("too many levels of symbolic links: " + path);
        }
        return p;
    }

    static String fit(String l, int recordLength) {
        if (l.length() >= recordLength) {
            return l.substring(0, recordLength);
        }
        return l + " ".repeat(recordLength - l.length());
    }
}
