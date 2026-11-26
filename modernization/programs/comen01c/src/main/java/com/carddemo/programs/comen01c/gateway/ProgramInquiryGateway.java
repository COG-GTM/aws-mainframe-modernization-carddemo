package com.carddemo.programs.comen01c.gateway;

/** EXEC CICS INQUIRE PROGRAM(name) NOHANDLE: {@code true} when EIBRESP = DFHRESP(NORMAL). */
public interface ProgramInquiryGateway {

    boolean isInstalled(String programName);
}
