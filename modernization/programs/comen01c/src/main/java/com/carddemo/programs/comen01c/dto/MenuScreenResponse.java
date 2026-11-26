package com.carddemo.programs.comen01c.dto;

import com.carddemo.programs.comen01c.copybook.Comen01Map;

import java.util.List;
import java.util.stream.IntStream;

/**
 * Output of map COMEN1A as sent by {@code EXEC CICS SEND MAP('COMEN1A') MAPSET('COMEN01') ERASE}.
 * String values are the raw O-field bytes; LOW-VALUES are {@code '\u0000'} characters.
 */
public record MenuScreenResponse(
        String trnnameO,
        String title01O,
        String curdateO,
        String pgmnameO,
        String title02O,
        String curtimeO,
        List<String> optnO,
        String optionO,
        String errmsgO,
        char errmsgC,
        boolean erase) {

    public static MenuScreenResponse from(Comen01Map map, boolean erase) {
        return new MenuScreenResponse(
                map.trnname().data(),
                map.title01().data(),
                map.curdate().data(),
                map.pgmname().data(),
                map.title02().data(),
                map.curtime().data(),
                IntStream.rangeClosed(1, Comen01Map.OPTION_LINES).mapToObj(i -> map.optn(i).data()).toList(),
                map.option().data(),
                map.errmsg().data(),
                map.errmsg().color(),
                erase);
    }

    /** OPTN001O..OPTN012O, 1-based. */
    public String optnO(int idx) {
        return optnO.get(idx - 1);
    }

    /** ERRMSGO without trailing spaces. */
    public String message() {
        return errmsgO.stripTrailing();
    }
}
