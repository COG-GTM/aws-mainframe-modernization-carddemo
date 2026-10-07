package com.carddemo.batch.load;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.batch.load.VsamDatasetLoader.Dataset;
import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.codec.RecordLayout;
import com.carddemo.common.data.CopybookRecordMapper;
import com.carddemo.support.PostgresRepositoryTest;
import com.carddemo.support.Samples;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Data migration round trip for every shipped sample: file → mapper → entity → PostgreSQL → entity → mapper → file
 * reproduces every stored field of each record byte for byte, with the deferred FKs checked.
 */
class VsamDatasetLoaderIT extends PostgresRepositoryTest {

    static final Map<Dataset, List<Dataset>> PARENTS = Map.of(
            Dataset.CARDXREF, List.of(Dataset.CUSTDATA, Dataset.ACCTDATA, Dataset.CARDDATA),
            Dataset.TRANCATG, List.of(Dataset.TRANTYPE));

    static Stream<Arguments> samples() {
        return Stream.concat(
                Samples.withEbcdicSample().stream().map(d -> Arguments.of(d, RecordEncoding.EBCDIC)),
                Samples.withAsciiSample().stream().map(d -> Arguments.of(d, RecordEncoding.ASCII)));
    }

    @ParameterizedTest(name = "{0} {1}")
    @MethodSource("samples")
    void everySampleRecordSurvivesTheDatabase(Dataset dataset, RecordEncoding encoding) throws Exception {
        for (Dataset parent : PARENTS.getOrDefault(dataset, List.of())) {
            loader.load(parent, Samples.path(parent, encoding), encoding);
        }
        List<FixedWidthRecord> original = Samples.read(dataset, encoding);
        assertThat(loader.load(dataset, Samples.path(dataset, encoding), encoding)).isEqualTo(original.size());
        flushAndClear();
        jdbc.execute("set constraints all immediate");

        CopybookRecordMapper<?> mapper = dataset.mapper();
        Class<?> entity = Class.forName(mapper.type().getName().replaceFirst("Record$", ""));
        List<?> rows = entityManager.getEntityManager()
                .createQuery("select e from " + entity.getSimpleName() + " e", entity).getResultList();
        assertThat(rows).hasSize(original.size());
        List<String> fromDb = rows.stream().map(row -> image(mapper, row, encoding)).sorted().toList();
        List<String> fromFile = original.stream().map(r -> withoutFiller(mapper, r)).sorted().toList();
        assertThat(fromDb).containsExactlyElementsOf(fromFile);
    }

    @SuppressWarnings("unchecked")
    private static <D extends Record> String image(CopybookRecordMapper<D> mapper, Object entity,
                                                   RecordEncoding encoding) {
        try {
            D value = (D) entity.getClass().getMethod("toRecord").invoke(entity);
            FixedWidthRecord record = FixedWidthRecord.spaces(mapper.layout(), encoding);
            mapper.writeInto(value, record);
            return record.text();
        } catch (ReflectiveOperationException e) {
            throw new IllegalStateException(e);
        }
    }

    /** The file image with FILLER blanked: FILLER is not stored (copybook-column-map.csv). */
    private static String withoutFiller(CopybookRecordMapper<?> mapper, FixedWidthRecord original) {
        byte[] image = FixedWidthRecord.spaces(mapper.layout(), original.encoding()).bytes();
        byte[] source = original.bytes();
        for (RecordLayout.Leaf leaf : mapper.layout().leaves()) {
            if (mapper.fieldToComponent().containsKey(leaf.key())) {
                System.arraycopy(source, leaf.field().offset(), image, leaf.field().offset(), leaf.field().size());
            }
        }
        return new FixedWidthRecord(mapper.layout(), image, original.encoding()).text();
    }
}
