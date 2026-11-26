package com.carddemo.programs.dbunldgs;

import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;

/** Run in a separate JVM: exits 0 if it can lock the file, 3 if another process holds it. */
public final class LockProbe {

    private LockProbe() {
    }

    public static void main(String[] args) throws Exception {
        try (FileChannel ch = FileChannel.open(Path.of(args[0]), StandardOpenOption.WRITE);
             FileLock lock = ch.tryLock()) {
            System.exit(lock == null ? 3 : 0);
        }
    }
}
