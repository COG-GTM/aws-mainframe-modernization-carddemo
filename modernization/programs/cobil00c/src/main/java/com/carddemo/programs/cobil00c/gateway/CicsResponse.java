package com.carddemo.programs.cobil00c.gateway;

import java.util.Optional;

/** RESP / RESP2 of a file-control command plus the record returned by INTO (when NORMAL). */
public record CicsResponse<T>(CicsResp resp, int resp2, Optional<T> record) {

    public static <T> CicsResponse<T> normal(T record) {
        return new CicsResponse<>(CicsResp.NORMAL, 0, Optional.of(record));
    }

    public static <T> CicsResponse<T> normal() {
        return new CicsResponse<>(CicsResp.NORMAL, 0, Optional.empty());
    }

    public static <T> CicsResponse<T> of(CicsResp resp, int resp2) {
        return new CicsResponse<>(resp, resp2, Optional.empty());
    }
}
