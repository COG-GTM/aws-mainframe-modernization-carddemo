package com.carddemo.data.repository;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.data.entity.CardXref;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

class CardXrefRepositoryIT extends RepositoryIT {

    @Autowired
    CardXrefRepository xrefs;

    @Test
    void readsByCardNumAndByAccountThroughCxacaix() {
        xrefs.save(Fixtures.xref("0500024453765740", 50, 50L));
        xrefs.save(Fixtures.xref("0500024453765739", 51, 50L));
        xrefs.save(Fixtures.xref("0683586198171516", 27, 27L));
        flushAndClear();

        var xref = xrefs.findById("0500024453765740").orElseThrow();
        assertThat(xref.getCustId()).isEqualTo(50);
        assertThat(xref.getAcctId()).isEqualTo(50L);
        assertThat(xrefs.findByAcctIdOrderByCardNum(50L))
                .extracting(CardXref::getCardNum, CardXref::getCustId)
                .containsExactly(
                        org.assertj.core.groups.Tuple.tuple("0500024453765739", 51),
                        org.assertj.core.groups.Tuple.tuple("0500024453765740", 50));
    }
}
