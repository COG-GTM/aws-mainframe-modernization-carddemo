package com.carddemo.transactiontype.domain.batch;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.transactiontype.domain.InMemoryTransactionTypeStore;
import java.util.List;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class BatchMaintenanceServiceTest {

    private final InMemoryTransactionTypeStore store = InMemoryTransactionTypeStore.seeded();
    private final BatchMaintenanceService batch = new BatchMaintenanceService(store);

    @Test
    @DisplayName("BR-TT-070/071 fixed 1+2+50 record layout; A, U, D and * actions")
    void br_tt_070_071_actions() {
        var r = batch.run(List.of("A08CASHBACK", "U02PAYMENT ONLINE", "D05", "*  just a comment"));
        assertThat(r.returnCode()).isZero();
        assertThat(r.inserted()).isEqualTo(1);
        assertThat(r.updated()).isEqualTo(1);
        assertThat(r.deleted()).isEqualTo(1);
        assertThat(r.ignored()).isEqualTo(1);
        assertThat(store.types).containsEntry("08", "CASHBACK").containsEntry("02", "PAYMENT ONLINE")
                .doesNotContainKey("05");
    }

    @Test
    @DisplayName("BR-TT-072 unknown action -> 'ERROR: TYPE NOT VALID', RC 4")
    void br_tt_072_invalidAction() {
        var r = batch.run(List.of("X09WHATEVER"));
        assertThat(r.returnCode()).isEqualTo(4);
        assertThat(r.sysout()).contains("ERROR: TYPE NOT VALID");
    }

    @Test
    @DisplayName("BR-TT-073 update/delete of missing key (+100) -> 'No records found.', RC 4")
    void br_tt_073_notFound() {
        var r = batch.run(List.of("U99NOPE", "D98"));
        assertThat(r.returnCode()).isEqualTo(4);
        assertThat(r.errors()).isEqualTo(2);
        assertThat(r.sysout()).filteredOn("No records found."::equals).hasSize(2);
    }

    @Test
    @DisplayName("BR-TT-074 negative SQLCODE -> message with SQLCODE, RC 4")
    void br_tt_074_sqlError() {
        var r = batch.run(List.of("A01DUPLICATE", "D01"));
        assertThat(r.sysout()).contains("Error accessing: TRANSACTION_TYPE table. SQLCODE:-803",
                "Error accessing: TRANSACTION_TYPE table. SQLCODE:-532");
        assertThat(r.returnCode()).isEqualTo(4);
    }

    @Test
    @DisplayName("BR-TT-075 errors do not stop the run; later records are still applied")
    void br_tt_075_continueAfterError() {
        var r = batch.run(List.of("X00BAD", "A09AFTER ERROR"));
        assertThat(r.returnCode()).isEqualTo(4);
        assertThat(store.types).containsEntry("09", "AFTER ERROR");
    }

    @Test
    @DisplayName("BR-TT-076 SYSOUT echoes each record and the action taken")
    void br_tt_076_sysout() {
        var r = batch.run(List.of("A08CASHBACK"));
        assertThat(r.sysout()).containsSubsequence("OPEN FILE OK", "PROCESSING   A08CASHBACK", "ADDING RECORD",
                "RECORD INSERTED SUCCESSFULLY");
    }
}
