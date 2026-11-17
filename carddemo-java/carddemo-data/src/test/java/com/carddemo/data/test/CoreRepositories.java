package com.carddemo.data.test;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.data.repository.AccountRepository;
import com.carddemo.data.repository.AppUserRepository;
import com.carddemo.data.repository.CardRepository;
import com.carddemo.data.repository.CardXrefRepository;
import com.carddemo.data.repository.CustomerRepository;
import com.carddemo.data.repository.DailyTransactionRepository;
import com.carddemo.data.repository.DisclosureGroupRepository;
import com.carddemo.data.repository.TransactionCategoryBalanceRepository;
import com.carddemo.data.repository.TransactionCategoryRepository;
import com.carddemo.data.repository.TransactionRepository;
import com.carddemo.data.repository.TransactionTypeRepository;
import jakarta.persistence.EntityManagerFactory;
import java.util.List;
import org.springframework.context.ApplicationContext;
import org.springframework.data.jpa.repository.JpaRepository;

/** Checks, from an application that depends on carddemo-data, that its entities and repositories are wired. */
public final class CoreRepositories {

    public static final List<Class<? extends JpaRepository<?, ?>>> ALL = List.of(
            AccountRepository.class, CardRepository.class, CardXrefRepository.class, CustomerRepository.class,
            TransactionRepository.class, DailyTransactionRepository.class,
            TransactionCategoryBalanceRepository.class, DisclosureGroupRepository.class,
            TransactionTypeRepository.class, TransactionCategoryRepository.class, AppUserRepository.class);

    private CoreRepositories() {
    }

    /** Every core repository is a bean that can query PostgreSQL, and every core entity is managed. */
    public static void assertLoaded(ApplicationContext context) {
        for (Class<? extends JpaRepository<?, ?>> type : ALL) {
            assertThat(context.getBean(type).count()).as(type.getSimpleName()).isZero();
        }
        assertThat(context.getBean(EntityManagerFactory.class).getMetamodel().getEntities())
                .filteredOn(e -> e.getJavaType().getPackageName().equals("com.carddemo.data.entity"))
                .hasSize(ALL.size());
    }
}
