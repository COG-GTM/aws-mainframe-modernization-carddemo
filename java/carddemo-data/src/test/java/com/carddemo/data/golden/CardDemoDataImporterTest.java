package com.carddemo.data.golden;

import com.carddemo.common.cobol.DataEncoding;
import com.carddemo.common.io.RecordFormat;
import com.carddemo.common.testing.CardDemoDataset;
import com.carddemo.common.testing.GoldenMaster;
import com.carddemo.common.testing.GoldenMasterTest;
import com.carddemo.common.testing.MainframeSources;
import com.carddemo.data.catalog.CardDemoFile;
import com.carddemo.data.load.CardDemoDataImporter;
import com.carddemo.data.repository.CardCrossReferenceRepository;
import com.carddemo.data.repository.CardRepository;
import com.carddemo.data.repository.DisclosureGroupRepository;
import com.carddemo.data.repository.TransactionCategoryBalanceRepository;
import com.carddemo.data.repository.UserSecurityRepository;
import com.carddemo.data.entity.DisclosureGroupId;
import com.carddemo.data.entity.UserSecurity;
import com.carddemo.data.entity.UserType;
import com.carddemo.data.testing.CardDemoDataTest;
import com.carddemo.data.testing.CardDemoFiles;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Loads every sample dataset into PostgreSQL through the entities and unloads it again: the unloaded records must
 * match the mainframe dataset field for field (FILLER is not persisted). Also exercises the VSAM-path repository
 * queries on the loaded data.
 */
@CardDemoDataTest
@GoldenMasterTest
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class CardDemoDataImporterTest {

    private static final Map<CardDemoDataset, Integer> EXPECTED_COUNTS = Map.of(
            CardDemoDataset.USRSEC, 10, CardDemoDataset.ACCTDATA, 50, CardDemoDataset.CARDDATA, 50,
            CardDemoDataset.CUSTDATA, 50, CardDemoDataset.CARDXREF, 50, CardDemoDataset.DALYTRAN, 300,
            CardDemoDataset.DISCGRP, 51, CardDemoDataset.TCATBALF, 50, CardDemoDataset.TRANCATG, 18,
            CardDemoDataset.TRANTYPE, 7);

    @Autowired
    CardDemoDataImporter importer;

    @Autowired
    List<JpaRepository<?, ?>> repositories;

    @Autowired
    CardRepository cards;

    @Autowired
    CardCrossReferenceRepository xrefs;

    @Autowired
    TransactionCategoryBalanceRepository balances;

    @Autowired
    DisclosureGroupRepository groups;

    @Autowired
    UserSecurityRepository users;

    @BeforeAll
    void loadAllSampleDatasets() {
        repositories.forEach(JpaRepository::deleteAllInBatch);
        MainframeSources sources = MainframeSources.locate();
        for (CardDemoDataset dataset : CardDemoDataset.values()) {
            int loaded = importer.load(CardDemoFiles.of(dataset), sources.ebcdicData(dataset.ebcdicFile()),
                    RecordFormat.FIXED, DataEncoding.EBCDIC);
            assertThat(loaded).as(dataset.name()).isEqualTo(EXPECTED_COUNTS.get(dataset));
        }
    }

    @ParameterizedTest(name = "{0}")
    @EnumSource(CardDemoDataset.class)
    void unloadedTableMatchesMainframeDataset(CardDemoDataset dataset, MainframeSources sources, GoldenMaster golden) {
        CardDemoFile file = CardDemoFiles.of(dataset);
        List<byte[]> unloaded = importer.unload(file, DataEncoding.EBCDIC);
        golden.assertMatches(golden.compare(dataset + " via PostgreSQL", file.layout(),
                dataset.ebcdicRecords(sources), DataEncoding.EBCDIC, unloaded, DataEncoding.EBCDIC, false));
    }

    @Test
    void asciiUnloadMatchesAsciiTransferForUntouchedRecords(MainframeSources sources, GoldenMaster golden) {
        List<byte[]> unloaded = importer.unload(CardDemoFile.CARD, DataEncoding.ASCII);
        golden.assertMatches(golden.compare("CARDDATA ASCII unload", CardDemoFile.CARD.layout(),
                CardDemoDataset.CARDDATA.asciiRecords(sources), DataEncoding.ASCII, unloaded, DataEncoding.ASCII, false));
    }

    @Test
    void alternateIndexPathsAreQueryable() {
        assertThat(cards.findByAccountIdOrderByCardNumber(1L)).isNotEmpty()
                .allSatisfy(c -> assertThat(c.getAccountId()).isEqualTo(1L));
        assertThat(xrefs.findByAccountId(1L)).isNotEmpty()
                .allSatisfy(x -> assertThat(x.getAccountId()).isEqualTo(1L));
        assertThat(balances.findByIdAccountIdOrderById(1L)).isNotEmpty();
        assertThat(cards.findByCardNumberGreaterThanEqualOrderByCardNumber("0", PageRequest.of(0, 7)).getContent())
                .hasSize(7);
    }

    @Test
    void compositeKeysAndEnumsArePersisted() {
        assertThat(groups.findById(new DisclosureGroupId("DEFAULT", "07", 1)))
                .hasValueSatisfying(g -> assertThat(g.getInterestRate()).isEqualByComparingTo("15.00"));
        assertThat(users.findByUserIdGreaterThanEqualOrderByUserId("USER", PageRequest.of(0, 10)).getContent())
                .hasSize(5)
                .extracting(UserSecurity::getUserType).containsOnly(UserType.USER);
    }
}
