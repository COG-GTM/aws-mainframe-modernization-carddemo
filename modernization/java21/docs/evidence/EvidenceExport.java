import com.carddemo.modernization.interest.*;
import com.carddemo.modernization.posting.TransactionPoster;
import com.carddemo.modernization.records.*;
import com.carddemo.modernization.records.codec.RecordCodec;
import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

/** Exports real service results; classification is deliberately independent of the Java codecs. */
public final class EvidenceExport {
    public static void main(String[] args) throws Exception {
        Path golden = Path.of(args[0]);
        Path output = Path.of(args[1]);
        Clock clock = Clock.fixed(Instant.parse(args[2]), ZoneOffset.UTC);
        Path posting = Files.createDirectories(output.resolve("posting"));
        Store store = new Store();
        for (AccountRecord a : read(golden.resolve("initial.ACCTFILE.bin"), AccountRecord.CODEC))
            store.accounts.put(a.acctId(), a);
        for (CardXrefRecord x : read(golden.resolve("initial.XREFFILE.bin"), CardXrefRecord.CODEC))
            store.cards.put(x.cardNumber(), x);
        for (TranCatBalanceRecord c : read(golden.resolve("initial.TCATBALF.bin"), TranCatBalanceRecord.CODEC))
            store.categories.put(key(c.accountId(), c.typeCode(), c.categoryCode()), c);
        for (byte[] t : split(Files.readAllBytes(golden.resolve("initial.TRANFILE.bin")), 350))
            store.transactions.put(new String(t, 0, 16, StandardCharsets.US_ASCII), t);
        TransactionPoster.Result result = new TransactionPoster(clock, StandardCharsets.US_ASCII)
                .post(split(Files.readAllBytes(golden.resolve("DALYTRAN.bin")), 350), store);
        Files.write(posting.resolve("CBTRN02C.TRANFILE.bin"), join(store.transactions.values()));
        Files.write(posting.resolve("DALYREJS.bin"), join(store.rejects));
        write(posting.resolve("CBTRN02C.ACCTFILE.bin"), store.accounts.values(), AccountRecord.CODEC);
        write(posting.resolve("CBTRN02C.TCATBALF.bin"), store.categories.values(), TranCatBalanceRecord.CODEC);
        Files.writeString(posting.resolve("result.json"), "{\"posted\":" + result.posted()
                + ",\"rejected\":" + result.rejected() + ",\"return_code\":" + result.returnCode() + "}\n");

        Path interest = Files.createDirectories(output.resolve("interest"));
        InterestRun run = new InterestCalculator(clock, args[3]).run(new InterestInput(
                read(golden.resolve("CBTRN02C.TCATBALF.bin"), TranCatBalanceRecord.CODEC),
                read(golden.resolve("CBTRN02C.ACCTFILE.bin"), AccountRecord.CODEC),
                read(golden.resolve("CBTRN02C.XREFFILE.bin"), CardXrefRecord.CODEC),
                read(golden.resolve("CBTRN02C.DISCGRP.bin"), DisclosureGroupRecord.CODEC)));
        write(interest.resolve("SYSTRAN.bin"), run.transactions(), TransactionRecord.CODEC);
        write(interest.resolve("CBACT04C.ACCTFILE.bin"), run.accounts(), AccountRecord.CODEC);
        Files.writeString(interest.resolve("CBACT04C.sysout.txt"), String.join("\n", run.sysout()) + "\n",
                StandardCharsets.ISO_8859_1);
    }

    private static String key(String account, String type, String category) { return account + type + category; }

    private static List<byte[]> split(byte[] bytes, int size) {
        if (bytes.length % size != 0) throw new IllegalArgumentException("Partial record");
        List<byte[]> rows = new ArrayList<>();
        for (int i = 0; i < bytes.length; i += size)
            rows.add(java.util.Arrays.copyOfRange(bytes, i, i + size));
        return rows;
    }

    private static <T> List<T> read(Path path, RecordCodec<T> codec) throws Exception {
        List<T> rows = new ArrayList<>();
        for (byte[] bytes : split(Files.readAllBytes(path), codec.length())) rows.add(codec.decode(bytes));
        return rows;
    }

    private static byte[] join(Iterable<byte[]> rows) {
        ByteArrayOutputStream bytes = new ByteArrayOutputStream();
        for (byte[] row : rows) bytes.writeBytes(row);
        return bytes.toByteArray();
    }

    private static <T> void write(Path path, Iterable<T> rows, RecordCodec<T> codec) throws Exception {
        ByteArrayOutputStream bytes = new ByteArrayOutputStream();
        for (T row : rows) bytes.writeBytes(codec.encode(row));
        Files.write(path, bytes.toByteArray());
    }

    private static final class Store implements TransactionPoster.Store {
        final Map<String, AccountRecord> accounts = new TreeMap<>();
        final Map<String, CardXrefRecord> cards = new TreeMap<>();
        final Map<String, TranCatBalanceRecord> categories = new TreeMap<>();
        final Map<String, byte[]> transactions = new TreeMap<>();
        final List<byte[]> rejects = new ArrayList<>();
        public void openOutputs() { transactions.clear(); rejects.clear(); }
        public CardXrefRecord findCard(String card) { return cards.get(card); }
        public AccountRecord findAccount(String account) { return accounts.get(account); }
        public TranCatBalanceRecord findCategory(String account, String type, String category) {
            return categories.get(key(account, type, category));
        }
        public void saveCategory(TranCatBalanceRecord c, boolean create) {
            categories.put(key(c.accountId(), c.typeCode(), c.categoryCode()), c);
        }
        public boolean rewriteAccount(AccountRecord a) { return accounts.replace(a.acctId(), a) != null; }
        public void writeTransaction(byte[] t) {
            String id = new String(t, 0, 16, StandardCharsets.US_ASCII);
            if (transactions.putIfAbsent(id, t) != null) throw new IllegalStateException("Duplicate transaction");
        }
        public void writeReject(byte[] r) { rejects.add(r); }
    }
}
