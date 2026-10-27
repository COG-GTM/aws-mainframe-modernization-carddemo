package com.carddemo.modernization.posting;

import com.carddemo.modernization.records.*;
import com.carddemo.modernization.records.codec.RecordCodec;
import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.*;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

class TransactionPosterTest {
    private static final Path GOLDEN = Path.of("../golden/expected");
    private static final TransactionPoster POSTER = new TransactionPoster(
            Clock.fixed(Instant.parse("2022-07-18T12:34:56Z"), ZoneOffset.UTC), StandardCharsets.US_ASCII);

    @Test void namedDifferential() throws Exception { differential("mixed", 2, 5); }
    @Test void volumeDifferential() throws Exception { differential("volume", 3000, 0); }

    private void differential(String name, int posted, int rejected) throws Exception {
        MemoryStore store = load(name);
        List<byte[]> input = split(Files.readAllBytes(GOLDEN.resolve(name).resolve("DALYTRAN.bin")), 350);
        TransactionPoster.Result result = POSTER.post(input, store);
        assertEquals(new TransactionPoster.Result(posted, rejected, rejected > 0 ? 4 : 0), result);
        String rc = Files.readString(GOLDEN.resolve(name).resolve("return-codes.json"));
        assertTrue(rc.contains("\"CBTRN02C\": " + result.returnCode()));
        compare(name, "CBTRN02C.TRANFILE.bin", encode(store.transactions.values(), TransactionRecord.CODEC));
        compare(name, "DALYREJS.bin", join(store.rejects));
        compare(name, "CBTRN02C.ACCTFILE.bin", encode(store.accounts.values(), AccountRecord.CODEC));
        compare(name, "CBTRN02C.TCATBALF.bin", encode(store.categories.values(), TranCatBalanceRecord.CODEC));
        Map<String, Integer> reasons = new TreeMap<>();
        for (byte[] reject : store.rejects) reasons.merge(RejectRecord.CODEC.decode(reject).reasonCode(), 1, Integer::sum);
        int compared = store.transactions.size() + store.rejects.size() + store.accounts.size() + store.categories.size();
        System.out.printf("DIFFERENTIAL %s: processed=%d records compared=%d (TRANSACT=%d DALYREJS=%d ACCTFILE=%d TCATBALF=%d) posted=%d rejected=%d reasons=%s RC=%d%n",
                name, input.size(), compared, store.transactions.size(), store.rejects.size(), store.accounts.size(),
                store.categories.size(), result.posted(), result.rejected(), reasons, result.returnCode());
    }

    @Test void emptyInput() {
        assertEquals(new TransactionPoster.Result(0, 0, 0), POSTER.post(List.of(), new MemoryStore()));
    }

    @Test void failedRewriteDoesNotRejectOrChangeReturnCode() throws Exception {
        MemoryStore store = load("mixed");
        byte[] input = firstPostedInput();
        Map<String, AccountRecord> original = new TreeMap<>(store.accounts);
        store.failRewrite = true;
        assertEquals(new TransactionPoster.Result(1, 0, 0), POSTER.post(List.of(input), store));
        assertEquals(original, store.accounts);
        assertEquals(1, store.transactions.size());
        assertTrue(store.rejects.isEmpty());
        assertFalse(store.categories.isEmpty());
    }

    @Test void transactionWriteFailureDoesNotRollBackEarlierUpdates() throws Exception {
        MemoryStore store = load("mixed");
        Map<String, AccountRecord> originalAccounts = new TreeMap<>(store.accounts);
        Map<String, TranCatBalanceRecord> originalCategories = new TreeMap<>(store.categories);
        store.failWrite = true;
        assertThrows(IllegalStateException.class, () -> POSTER.post(List.of(firstPostedInput()), store));
        assertNotEquals(originalAccounts, store.accounts);
        assertNotEquals(originalCategories, store.categories);
        assertTrue(store.transactions.isEmpty());
        assertTrue(store.rejects.isEmpty());
    }

    @Test void malformedRecordFailsRatherThanSkipping() {
        assertThrows(IllegalArgumentException.class, () -> POSTER.post(List.of(new byte[349]), new MemoryStore()));
    }

    @Test void malformedNumericFieldsDoNotPreventLookupRejects() throws Exception {
        for (String reason : List.of("0100", "0101")) {
            MemoryStore store = load("mixed");
            byte[] input = firstPostedInput().clone();
            Arrays.fill(input, 143, 152, (byte) ' ');
            Arrays.fill(input, 132, 143, (byte) ' ');
            if (reason.equals("0100")) store.cards.clear();
            else store.accounts.clear();
            assertEquals(new TransactionPoster.Result(0, 1, 4), POSTER.post(List.of(input), store));
            byte[] reject = store.rejects.getFirst();
            assertEquals(430, reject.length);
            assertArrayEquals(input, Arrays.copyOf(reject, 350));
            assertEquals(reason, RejectRecord.CODEC.decode(reject).reasonCode());
        }
    }

    @Test void malformedFieldsAfterSuccessfulLookupsFailWithoutCoercion() throws Exception {
        for (int offset : List.of(132, 143)) {
            MemoryStore store = load("mixed");
            byte[] input = firstPostedInput().clone();
            Arrays.fill(input, offset, offset + (offset == 132 ? 11 : 9), (byte) ' ');
            assertThrows(IllegalArgumentException.class, () -> POSTER.post(List.of(input), store));
            assertTrue(store.rejects.isEmpty());
            assertTrue(store.transactions.isEmpty());
        }
    }

    @Test void rejectReasonAndTimestampAreLocaleIndependent() throws Exception {
        Locale original = Locale.getDefault();
        try {
            Locale.setDefault(Locale.forLanguageTag("ar-EG"));
            MemoryStore store = load("mixed");
            byte[] input = firstPostedInput();
            assertEquals(new TransactionPoster.Result(1, 0, 0), POSTER.post(List.of(input), store));
            assertEquals("2022-07-18-12.34.56.000000", store.transactions.values().iterator().next().processedTimestamp());
            store.cards.clear();
            assertEquals(new TransactionPoster.Result(0, 1, 4), POSTER.post(List.of(input), store));
            assertEquals("0100", RejectRecord.CODEC.decode(store.rejects.getFirst()).reasonCode());
        } finally {
            Locale.setDefault(original);
        }
    }

    private byte[] firstPostedInput() throws Exception {
        Set<String> posted = new HashSet<>();
        for (byte[] b : split(Files.readAllBytes(GOLDEN.resolve("mixed/CBTRN02C.TRANFILE.bin")), 350))
            posted.add(TransactionRecord.CODEC.decode(b).id());
        return split(Files.readAllBytes(GOLDEN.resolve("mixed/DALYTRAN.bin")), 350).stream()
                .filter(b -> posted.contains(DailyTransactionRecord.CODEC.decode(b).id())).findFirst().orElseThrow();
    }

    private static MemoryStore load(String name) throws Exception {
        MemoryStore store = new MemoryStore();
        Path path = GOLDEN.resolve(name);
        for (byte[] b : split(Files.readAllBytes(path.resolve("initial.ACCTFILE.bin")), 300)) {
            AccountRecord a = AccountRecord.CODEC.decode(b); store.accounts.put(a.acctId(), a);
        }
        for (byte[] b : split(Files.readAllBytes(path.resolve("initial.XREFFILE.bin")), 50)) {
            CardXrefRecord x = CardXrefRecord.CODEC.decode(b); store.cards.put(x.cardNumber(), x);
        }
        for (byte[] b : split(Files.readAllBytes(path.resolve("initial.TCATBALF.bin")), 50)) {
            TranCatBalanceRecord c = TranCatBalanceRecord.CODEC.decode(b);
            store.categories.put(c.accountId() + c.typeCode() + c.categoryCode(), c);
        }
        return store;
    }

    private static List<byte[]> split(byte[] bytes, int length) {
        assertEquals(0, bytes.length % length);
        List<byte[]> records = new ArrayList<>();
        for (int i = 0; i < bytes.length; i += length) records.add(Arrays.copyOfRange(bytes, i, i + length));
        return records;
    }

    private static byte[] join(Collection<byte[]> records) {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        records.forEach(out::writeBytes);
        return out.toByteArray();
    }

    private static <T> byte[] encode(Collection<T> records, RecordCodec<T> codec) {
        return join(records.stream().map(codec::encode).toList());
    }

    private static void compare(String set, String file, byte[] actual) throws Exception {
        assertArrayEquals(Files.readAllBytes(GOLDEN.resolve(set).resolve(file)), actual, set + "/" + file);
    }

    private static class MemoryStore implements TransactionPoster.Store {
        final Map<String, CardXrefRecord> cards = new TreeMap<>();
        final Map<String, AccountRecord> accounts = new TreeMap<>();
        final Map<String, TranCatBalanceRecord> categories = new TreeMap<>();
        final Map<String, TransactionRecord> transactions = new TreeMap<>();
        final List<byte[]> rejects = new ArrayList<>();
        boolean failRewrite;
        boolean failWrite;
        public CardXrefRecord findCard(String key) { return cards.get(key); }
        public AccountRecord findAccount(String key) { return accounts.get(key); }
        public TranCatBalanceRecord findCategory(String account, String type, String category) {
            return categories.get(account + type + category);
        }
        public void saveCategory(TranCatBalanceRecord c, boolean create) {
            String key = c.accountId() + c.typeCode() + c.categoryCode();
            assertEquals(create, !categories.containsKey(key));
            categories.put(key, c);
        }
        public boolean rewriteAccount(AccountRecord a) {
            if (failRewrite) return false;
            return accounts.replace(a.acctId(), a) != null;
        }
        public void writeTransaction(TransactionRecord t) {
            if (failWrite || transactions.putIfAbsent(t.id(), t) != null) throw new IllegalStateException("Transaction write failed");
        }
        public void writeReject(byte[] record) { rejects.add(record); }
    }
}
