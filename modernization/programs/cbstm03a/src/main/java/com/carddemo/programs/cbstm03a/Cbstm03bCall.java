package com.carddemo.programs.cbstm03a;

import com.carddemo.programs.cbstm03a.copybook.M03bArea;

/** {@code CALL 'CBSTM03B' USING WS-M03B-AREA}: the file-access subroutine CBSTM03A delegates all input I/O to. */
@FunctionalInterface
public interface Cbstm03bCall {

    void call(M03bArea area);
}
