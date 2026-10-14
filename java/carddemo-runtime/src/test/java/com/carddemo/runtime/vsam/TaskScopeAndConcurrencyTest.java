package com.carddemo.runtime.vsam;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.runtime.cics.CicsResp;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.atomic.AtomicReference;
import org.h2.jdbcx.JdbcDataSource;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

class TaskScopeAndConcurrencyTest {

    @AfterEach
    void endTask() {
        TaskFileState.endTask();
    }

    private static KsdsFile file() {
        KsdsFile f = KsdsFile.inMemory("F", 6, 1, 3);
        f.load(List.of("xAAA01", "xBBB01", "xDDD01"));
        return f;
    }

    @Test
    void rewriteAfterHeldRecordWasDeletedIsNotfound() {
        KsdsFile f = file();
        assertThat(f.readForUpdate("AAA").isNormal()).isTrue();
        f.store().delete("AAA".getBytes(java.nio.charset.StandardCharsets.ISO_8859_1));
        FileResult r = f.rewrite("zAAA02");
        assertThat(r.resp()).isEqualTo(CicsResp.NOTFND);
        assertThat(f.read("AAA").resp()).isEqualTo(CicsResp.NOTFND);
    }

    @Test
    void browseReadsHonourFileState() {
        KsdsFile f = file();
        AlternateIndexPath aix = f.path("AIX", 4, 2, false);
        assertThat(f.startbr("AAA").isNormal()).isTrue();
        assertThat(aix.startbr("01").isNormal()).isTrue();
        f.setState(FileState.DISABLED);
        assertThat(f.readnext().resp()).isEqualTo(CicsResp.DISABLED);
        assertThat(f.readprev().resp()).isEqualTo(CicsResp.DISABLED);
        assertThat(aix.readnext().resp()).isEqualTo(CicsResp.DISABLED);
        f.setState(FileState.CLOSED);
        assertThat(f.readnext().resp()).isEqualTo(CicsResp.NOTOPEN);
    }

    @Test
    void browsesAndHoldsAreTaskScoped() throws Exception {
        KsdsFile f = file();
        assertThat(f.startbr("AAA").isNormal()).isTrue();
        assertThat(f.readForUpdate("AAA").isNormal()).isTrue();
        AtomicReference<String> other = new AtomicReference<>();
        Thread t = new Thread(() -> {
            f.startbr("DDD");
            f.readForUpdate("DDD");
            other.set(f.readnext().key() + f.rewrite("yAAA02").resp());
        });
        t.start();
        t.join();
        assertThat(other.get()).isEqualTo("DDDINVREQ");
        assertThat(f.readnext().key()).isEqualTo("AAA");
        assertThat(f.rewrite("yAAA02").isNormal()).isTrue();

        TaskFileState.endTask();
        assertThat(f.readnext().resp()).isEqualTo(CicsResp.INVREQ);
        assertThat(f.rewrite("yAAA03").resp()).isEqualTo(CicsResp.INVREQ);
    }

    @Test
    void concurrentDuplicateJdbcInsertsYieldOneWinnerAndNoExceptions() throws Exception {
        JdbcDataSource ds = new JdbcDataSource();
        ds.setURL("jdbc:h2:mem:race;DB_CLOSE_DELAY=-1");
        JdbcKsdsStore store = new JdbcKsdsStore(ds, "RACE");
        byte[] key = {'K'};
        int n = 8;
        CountDownLatch go = new CountDownLatch(1);
        ExecutorService pool = Executors.newFixedThreadPool(n);
        List<Future<Boolean>> results = new ArrayList<>();
        for (int i = 0; i < n; i++) {
            results.add(pool.submit(() -> {
                go.await();
                return store.insert(key, new byte[] {'K', 'V'});
            }));
        }
        go.countDown();
        int wins = 0;
        for (Future<Boolean> r : results) {
            wins += r.get() ? 1 : 0;
        }
        pool.shutdown();
        assertThat(wins).isEqualTo(1);
    }
}
