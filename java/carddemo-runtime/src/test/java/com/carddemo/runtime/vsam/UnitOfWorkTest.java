package com.carddemo.runtime.vsam;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.runtime.cics.CicsResp;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

class UnitOfWorkTest {

    private static KsdsFile file(boolean recoverable) {
        KsdsFile f = KsdsFile.inMemory("TEST", 6, 0, 3);
        f.load(java.util.List.of("AAA001", "BBB002"));
        f.setRecoverable(recoverable);
        return f;
    }

    @AfterEach
    void endTask() {
        TaskFileState.endTask();
    }

    @Test
    void rollbackRestoresRecoverableUpdates() {
        KsdsFile f = file(true);
        assertThat(f.readForUpdate("AAA").resp()).isEqualTo(CicsResp.NORMAL);
        assertThat(f.rewrite("AAA999").resp()).isEqualTo(CicsResp.NORMAL);
        assertThat(f.write("CCC003").resp()).isEqualTo(CicsResp.NORMAL);
        assertThat(f.delete("BBB").resp()).isEqualTo(CicsResp.NORMAL);
        UnitOfWork.rollback();
        assertThat(f.read("AAA").dataString()).isEqualTo("AAA001");
        assertThat(f.read("BBB").dataString()).isEqualTo("BBB002");
        assertThat(f.read("CCC").resp()).isEqualTo(CicsResp.NOTFND);
    }

    @Test
    void rollbackDoesNotBackOutNonRecoverableFilesButReleasesLocks() {
        KsdsFile f = file(false);
        f.readForUpdate("AAA");
        f.rewrite("AAA999");
        f.readForUpdate("BBB");
        UnitOfWork.rollback();
        assertThat(f.read("AAA").dataString()).isEqualTo("AAA999");
        assertThat(f.rewrite("BBB777").resp()).isEqualTo(CicsResp.INVREQ);
    }

    @Test
    void commitKeepsUpdatesAndReleasesLocks() {
        KsdsFile f = file(true);
        f.readForUpdate("AAA");
        f.rewrite("AAA555");
        f.readForUpdate("BBB");
        UnitOfWork.commit();
        UnitOfWork.rollback();
        assertThat(f.read("AAA").dataString()).isEqualTo("AAA555");
        assertThat(f.rewrite("BBB777").resp()).isEqualTo(CicsResp.INVREQ);
    }
}
