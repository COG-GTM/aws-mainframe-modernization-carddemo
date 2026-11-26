package com.carddemo.programs.cobswait;

import com.carddemo.programs.cobswait.copybook.MvswaitTime;

/**
 * Gateway for {@code CALL 'MVSWAIT' USING MVSWAIT-TIME} (app/asm/MVSWAIT.asm): waits for the given
 * number of centiseconds and returns the routine's R15 return code.
 */
@FunctionalInterface
public interface MvsWait {

    int call(MvswaitTime time);
}
