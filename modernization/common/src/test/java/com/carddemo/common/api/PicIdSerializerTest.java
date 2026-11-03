package com.carddemo.common.api;

import static org.assertj.core.api.Assertions.assertThat;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;

class PicIdSerializerTest {

    record Ids(@PicId(digits = 11) Long accountId, @PicId(digits = 9) Long customerId, Long plain) {
    }

    @Test
    void writesPicIdsAsZeroPaddedStrings() throws Exception {
        String json = new ObjectMapper().writeValueAsString(new Ids(1L, 42L, 7L));

        assertThat(json).isEqualTo("{\"accountId\":\"00000000001\",\"customerId\":\"000000042\",\"plain\":7}");
    }

    @Test
    void leavesNullIdsNull() throws Exception {
        assertThat(new ObjectMapper().writeValueAsString(new Ids(null, null, null)))
                .isEqualTo("{\"accountId\":null,\"customerId\":null,\"plain\":null}");
    }
}
