package com.carddemo.batch.posttran.cobol;

import java.io.PrintStream;
import org.springframework.stereotype.Component;

/** SYSOUT DD: the target of COBOL DISPLAY statements. */
@Component
public class Sysout {

    private volatile PrintStream out = System.out;

    public void display(String line) {
        out.println(line);
    }

    public void redirect(PrintStream target) {
        this.out = target;
    }
}
