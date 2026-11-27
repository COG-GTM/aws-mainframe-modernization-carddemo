package com.carddemo.data.repository;

import com.carddemo.common.cobol.DataEncoding;
import com.carddemo.common.codec.RecordCodec;
import com.carddemo.data.catalog.CopybookCatalog;
import com.carddemo.data.entity.PendingAuthorizationDetail;
import com.carddemo.data.entity.PendingAuthorizationSummary;
import com.carddemo.data.mapper.PendingAuthorizationDetailMapper;
import com.carddemo.data.mapper.PendingAuthorizationSummaryMapper;
import com.carddemo.data.record.PendingAuthDetailRecord;
import com.carddemo.data.record.PendingAuthSummaryRecord;
import com.carddemo.data.testing.CardDemoDataTest;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

/** COMP-3 / COMP / OCCURS segments of the optional IMS module, from bytes to PostgreSQL and back. */
@CardDemoDataTest
@Transactional
class PendingAuthorizationPersistenceTest {

    @Autowired
    PendingAuthorizationSummaryRepository summaries;

    @Autowired
    PendingAuthorizationDetailRepository details;

    @Autowired
    PendingAuthorizationSummaryMapper summaryMapper;

    @Autowired
    PendingAuthorizationDetailMapper detailMapper;

    @Test
    void summaryAndDetailSegmentsRoundTrip() {
        RecordCodec<PendingAuthSummaryRecord> summaryCodec =
                RecordCodec.of(PendingAuthSummaryRecord.class, CopybookCatalog.layout("CIPAUSMY"), DataEncoding.EBCDIC);
        RecordCodec<PendingAuthDetailRecord> detailCodec =
                RecordCodec.of(PendingAuthDetailRecord.class, CopybookCatalog.layout("CIPAUDTY"), DataEncoding.EBCDIC);

        PendingAuthSummaryRecord summary = new PendingAuthSummaryRecord(41000000001L, 123456789L, "A",
                List.of("A1", "B2", "C3", "", "Z9"), new BigDecimal("5000.00"), new BigDecimal("1000.00"),
                new BigDecimal("-12.34"), new BigDecimal("0.00"), 3, 1, new BigDecimal("812.40"), new BigDecimal("99.99"));
        PendingAuthDetailRecord detail = new PendingAuthDetailRecord(new PendingAuthDetailRecord.Key(73682, 813595999L),
                "260318", "184004", "4000000000000001", "AUTH", "1227", "0100", "POS", "A1B2C3", "00", "0000",
                0, new BigDecimal("125.50"), new BigDecimal("125.50"), "5411", "840", 5, "MERCH000000001",
                "CORNER GROCERY", "SPRINGFIELD", "IL", "62701", "TXN000000000001", "P", "", "");

        byte[] summaryBytes = summaryCodec.encode(summary);
        byte[] detailBytes = detailCodec.encode(detail);
        assertThat(summaryBytes).hasSize(100);
        assertThat(detailBytes).hasSize(200);

        PendingAuthorizationSummary entity = summaryMapper.toEntity(summaryCodec.decode(summaryBytes));
        summaries.saveAndFlush(entity);
        details.saveAndFlush(detailMapper.toEntity(detailCodec.decode(detailBytes), entity.getAccountId()));

        PendingAuthorizationSummary reloaded = summaries.findById(41000000001L).orElseThrow();
        assertThat(reloaded.getAccountStatuses()).containsExactly("A1", "B2", "C3", "", "Z9");
        assertThat(reloaded.getCreditBalance()).isEqualByComparingTo("-12.34");
        List<PendingAuthorizationDetail> children = details.findByIdAccountIdOrderById(41000000001L);
        assertThat(children).singleElement().satisfies(d -> {
            assertThat(d.getId().getAuthDateKey()).isEqualTo(73682);
            assertThat(d.getTransactionAmount()).isEqualByComparingTo("125.50");
        });

        assertThat(summaryCodec.encode(summaryMapper.toRecord(reloaded))).isEqualTo(summaryBytes);
        assertThat(detailCodec.encode(detailMapper.toRecord(children.get(0)))).isEqualTo(detailBytes);
    }
}
