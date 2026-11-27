package com.carddemo.data.golden;

import com.carddemo.common.cobol.DataEncoding;
import com.carddemo.common.codec.RecordCodec;
import com.carddemo.common.testing.CardDemoDataset;
import com.carddemo.common.testing.GoldenMaster;
import com.carddemo.common.testing.GoldenMasterTest;
import com.carddemo.common.testing.MainframeSources;
import com.carddemo.data.catalog.CardDemoFile;
import com.carddemo.data.entity.Account;
import com.carddemo.data.entity.Customer;
import com.carddemo.data.entity.DailyTransaction;
import com.carddemo.data.entity.UserType;
import com.carddemo.data.mapper.AccountMapper;
import com.carddemo.data.mapper.CardCrossReferenceMapper;
import com.carddemo.data.mapper.CardMapper;
import com.carddemo.data.mapper.CustomerMapper;
import com.carddemo.data.mapper.DailyTransactionMapper;
import com.carddemo.data.mapper.DisclosureGroupMapper;
import com.carddemo.data.mapper.TransactionCategoryBalanceMapper;
import com.carddemo.data.mapper.TransactionCategoryMapper;
import com.carddemo.data.mapper.TransactionTypeMapper;
import com.carddemo.data.mapper.UserSecurityMapper;
import com.carddemo.data.record.AccountRecord;
import com.carddemo.data.record.CustomerRecord;
import com.carddemo.data.record.DailyTransactionRecord;
import com.carddemo.data.record.UserSecurityRecord;
import com.carddemo.data.testing.CardDemoFiles;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.mapstruct.factory.Mappers;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.function.UnaryOperator;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Golden master without a database: every sample record is decoded with its copybook, mapped to the JPA entity,
 * mapped back and re-encoded; all fields must match the original EBCDIC record.
 */
@GoldenMasterTest
class RecordMappingGoldenMasterTest {

    @SuppressWarnings({"unchecked", "rawtypes"})
    private static UnaryOperator<Record> entityRoundTrip(CardDemoFile file) {
        return switch (file) {
            case USER_SECURITY -> via(Mappers.getMapper(UserSecurityMapper.class)::toEntity, Mappers.getMapper(UserSecurityMapper.class)::toRecord);
            case ACCOUNT -> via(Mappers.getMapper(AccountMapper.class)::toEntity, Mappers.getMapper(AccountMapper.class)::toRecord);
            case CARD -> via(Mappers.getMapper(CardMapper.class)::toEntity, Mappers.getMapper(CardMapper.class)::toRecord);
            case CUSTOMER -> via(Mappers.getMapper(CustomerMapper.class)::toEntity, Mappers.getMapper(CustomerMapper.class)::toRecord);
            case CARD_XREF -> via(Mappers.getMapper(CardCrossReferenceMapper.class)::toEntity, Mappers.getMapper(CardCrossReferenceMapper.class)::toRecord);
            case DAILY_TRANSACTION -> via(Mappers.getMapper(DailyTransactionMapper.class)::toEntity, Mappers.getMapper(DailyTransactionMapper.class)::toRecord);
            case TRAN_CAT_BALANCE -> via(Mappers.getMapper(TransactionCategoryBalanceMapper.class)::toEntity, Mappers.getMapper(TransactionCategoryBalanceMapper.class)::toRecord);
            case DISCLOSURE_GROUP -> via(Mappers.getMapper(DisclosureGroupMapper.class)::toEntity, Mappers.getMapper(DisclosureGroupMapper.class)::toRecord);
            case TRANSACTION_TYPE -> via(Mappers.getMapper(TransactionTypeMapper.class)::toEntity, Mappers.getMapper(TransactionTypeMapper.class)::toRecord);
            case TRANSACTION_CATEGORY -> via(Mappers.getMapper(TransactionCategoryMapper.class)::toEntity, Mappers.getMapper(TransactionCategoryMapper.class)::toRecord);
            case TRANSACTION -> throw new IllegalArgumentException("No TRANSACT sample dataset");
        };
    }

    @SuppressWarnings({"unchecked", "rawtypes"})
    private static <R extends Record, E> UnaryOperator<Record> via(java.util.function.Function<R, E> toEntity,
                                                                   java.util.function.Function<E, R> toRecord) {
        return r -> toRecord.apply(toEntity.apply((R) r));
    }

    @ParameterizedTest(name = "{0}")
    @EnumSource(CardDemoDataset.class)
    void recordsSurviveEntityRoundTrip(CardDemoDataset dataset, MainframeSources sources, GoldenMaster golden) {
        CardDemoFile file = CardDemoFiles.of(dataset);
        RecordCodec<Record> codec = file.codec(DataEncoding.EBCDIC);
        UnaryOperator<Record> roundTrip = entityRoundTrip(file);
        List<byte[]> original = dataset.ebcdicRecords(sources);
        List<byte[]> rewritten = new ArrayList<>();
        for (byte[] record : original) {
            rewritten.add(codec.encode(roundTrip.apply(codec.decode(record))));
        }
        golden.assertMatches(golden.compare(dataset + " entity round trip", file.layout(),
                original, DataEncoding.EBCDIC, rewritten, DataEncoding.EBCDIC, false));
    }

    @Test
    void typedValuesFromFirstRecords(MainframeSources sources) {
        AccountRecord accountRecord = CardDemoFile.ACCOUNT.<AccountRecord>codec(DataEncoding.EBCDIC)
                .decode(CardDemoDataset.ACCTDATA.ebcdicRecords(sources).get(0));
        Account account = Mappers.getMapper(AccountMapper.class).toEntity(accountRecord);
        assertThat(account.getId()).isEqualTo(1L);
        assertThat(account.getCurrentBalance()).isEqualByComparingTo("194.00");
        assertThat(account.getCreditLimit()).isEqualByComparingTo("2020.00");
        assertThat(account.getOpenDate()).isEqualTo(LocalDate.of(2014, 11, 20));

        CustomerRecord customerRecord = CardDemoFile.CUSTOMER.<CustomerRecord>codec(DataEncoding.EBCDIC)
                .decode(CardDemoDataset.CUSTDATA.ebcdicRecords(sources).get(0));
        Customer customer = Mappers.getMapper(CustomerMapper.class).toEntity(customerRecord);
        assertThat(customer.getFirstName()).isEqualTo("Immanuel");
        assertThat(customer.getAddress().getLine1()).isEqualTo("618 Deshaun Route");
        assertThat(customer.getDateOfBirth()).isEqualTo(LocalDate.of(1961, 6, 8));

        DailyTransactionRecord txRecord = CardDemoFile.DAILY_TRANSACTION.<DailyTransactionRecord>codec(DataEncoding.EBCDIC)
                .decode(CardDemoDataset.DALYTRAN.ebcdicRecords(sources).get(0));
        DailyTransaction tx = Mappers.getMapper(DailyTransactionMapper.class).toEntity(txRecord);
        assertThat(tx.getAmount()).isEqualByComparingTo("504.77");
        assertThat(tx.getOriginTimestamp()).isEqualTo(LocalDateTime.of(2022, 6, 10, 19, 27, 53));
        assertThat(tx.getProcessedTimestamp()).isNull();

        UserSecurityRecord user = CardDemoFile.USER_SECURITY.<UserSecurityRecord>codec(DataEncoding.EBCDIC)
                .decode(CardDemoDataset.USRSEC.ebcdicRecords(sources).get(0));
        assertThat(Mappers.getMapper(UserSecurityMapper.class).toEntity(user).getUserType()).isEqualTo(UserType.ADMIN);
    }
}
