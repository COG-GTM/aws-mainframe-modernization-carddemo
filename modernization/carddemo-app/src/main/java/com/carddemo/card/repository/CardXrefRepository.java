package com.carddemo.card.repository;

import com.carddemo.card.domain.CardXref;
import org.springframework.data.jpa.repository.JpaRepository;

/** Table {@code card.card_xref} (CARDXREF). */
public interface CardXrefRepository extends JpaRepository<CardXref, String> {
}
