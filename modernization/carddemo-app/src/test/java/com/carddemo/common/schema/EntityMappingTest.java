package com.carddemo.common.schema;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.batch.BatchOutputFile;
import com.carddemo.batch.load.VsamDatasetLoader.Dataset;
import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.data.CopybookRecordMapper;
import com.carddemo.common.schema.CopybookColumnMap.Entry;
import com.carddemo.support.Samples;
import jakarta.persistence.Column;
import jakarta.persistence.EmbeddedId;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import jakarta.persistence.Version;
import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * s3.2 acceptance, per table: the record mapper binds exactly the stored fields of copybook-column-map.csv, the
 * entity stores each one in the CSV column of the CSV table, and every sample record survives
 * fixed-width → record → fixed-width byte for byte.
 */
class EntityMappingTest {

    static final Set<String> VERSIONED = Set.of("user_security", "customer", "account", "card");

    static Class<?> entityOf(CopybookRecordMapper<?> mapper) throws ClassNotFoundException {
        return Class.forName(mapper.type().getName().replaceFirst("Record$", ""));
    }

    static Field entityField(Class<?> entity, String name) {
        for (Field f : entity.getDeclaredFields()) {
            if (f.getName().equals(name)) {
                return f;
            }
            if (f.isAnnotationPresent(EmbeddedId.class)) {
                Field inId = Arrays.stream(f.getType().getDeclaredFields()).filter(k -> k.getName().equals(name))
                        .findFirst().orElse(null);
                if (inId != null) {
                    return inId;
                }
            }
        }
        throw new AssertionError(entity.getSimpleName() + " has no field " + name);
    }

    @ParameterizedTest
    @EnumSource(Dataset.class)
    void mapperBindsExactlyTheStoredCsvFieldsInStorageOrder(Dataset dataset) {
        CopybookRecordMapper<?> mapper = dataset.mapper();
        List<String> stored = CopybookColumnMap.forCopybook(mapper.copybook()).stream().filter(Entry::stored)
                .map(Entry::field).toList();
        assertThat(mapper.fieldToComponent().keySet()).containsExactlyElementsOf(stored);
        assertThat(CopybookColumnMap.forCopybook(mapper.copybook()).get(0).dataset()).isEqualTo(dataset.name());
    }

    @ParameterizedTest
    @EnumSource(Dataset.class)
    void entityStoresEachFieldInItsCsvColumn(Dataset dataset) throws Exception {
        CopybookRecordMapper<?> mapper = dataset.mapper();
        Class<?> entity = entityOf(mapper);
        Map<String, String> columnByField = new LinkedHashMap<>();
        String table = null;
        for (Entry e : CopybookColumnMap.forCopybook(mapper.copybook())) {
            if (e.stored()) {
                columnByField.put(e.field(), e.column());
                table = e.table();
            }
        }
        assertThat(entity.isAnnotationPresent(Entity.class)).isTrue();
        assertThat(entity.getAnnotation(Table.class).name()).isEqualTo(table);
        mapper.fieldToComponent().forEach((cobol, component) -> {
            Column column = entityField(entity, component).getAnnotation(Column.class);
            assertThat(column).as(entity.getSimpleName() + "." + component).isNotNull();
            assertThat(column.name()).as(cobol).isEqualTo(columnByField.get(cobol));
        });
        boolean versioned = Arrays.stream(entity.getDeclaredFields()).anyMatch(f -> f.isAnnotationPresent(
                Version.class));
        assertThat(versioned).as(table + " @Version").isEqualTo(VERSIONED.contains(table));
    }

    @Test
    void everySchemaTableHasAnEntity() throws Exception {
        Set<String> tables = Stream.concat(Arrays.stream(Dataset.values()).map(d -> {
            try {
                return entityOf(d.mapper()).getAnnotation(Table.class).name();
            } catch (ClassNotFoundException e) {
                throw new IllegalStateException(e);
            }
        }), Stream.of(BatchOutputFile.class.getAnnotation(Table.class).name())).collect(Collectors.toSet());
        assertThat(tables).containsExactlyInAnyOrder("user_security", "customer", "account", "card", "card_xref",
                "transaction_type", "transaction_category", "disclosure_group", "tran_cat_balance", "transaction",
                "daily_transaction", "batch_output_file");
    }

    static Stream<Arguments> samples() {
        return Stream.concat(
                Samples.withEbcdicSample().stream().map(d -> Arguments.of(d, RecordEncoding.EBCDIC)),
                Samples.withAsciiSample().stream().map(d -> Arguments.of(d, RecordEncoding.ASCII)));
    }

    @ParameterizedTest(name = "{0} {1}")
    @MethodSource("samples")
    void sampleRecordsRoundTripByteForByte(Dataset dataset, RecordEncoding encoding) {
        roundTrip(dataset.mapper(), Samples.read(dataset, encoding));
    }

    @Test
    void transactRoundTripsThroughDailyTransactionImages() {
        List<FixedWidthRecord> daily = Samples.read(Dataset.DALYTRAN, RecordEncoding.EBCDIC);
        List<FixedWidthRecord> asTransact = daily.stream()
                .map(r -> r.as(Dataset.TRANSACT.mapper().layout())).toList();
        roundTrip(Dataset.TRANSACT.mapper(), asTransact);
    }

    private static <D extends Record> void roundTrip(CopybookRecordMapper<D> mapper, List<FixedWidthRecord> records) {
        assertThat(records).isNotEmpty();
        for (int i = 0; i < records.size(); i++) {
            FixedWidthRecord original = records.get(i);
            D value = mapper.fromRecord(original);
            FixedWidthRecord rewritten = original.copy();
            mapper.writeInto(value, rewritten);
            assertThat(rewritten.bytes()).as("%s record %d", mapper.copybook(), i + 1)
                    .isEqualTo(original.bytes());
            assertThat(mapper.fromRecord(mapper.toRecord(value, original.encoding()))).isEqualTo(value);
        }
    }
}
