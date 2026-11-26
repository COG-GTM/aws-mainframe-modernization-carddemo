package com.carddemo.programs.cousr01c.gateway;

import com.carddemo.programs.cousr01c.copybook.SecUserData;

/** USRSEC VSAM KSDS (dataset 'USRSEC  ', key SEC-USR-ID, record length 80). */
public interface UserSecRepository {

    /** EXEC CICS WRITE DATASET('USRSEC') FROM(SEC-USER-DATA) RIDFLD(SEC-USR-ID). */
    WriteResult write(SecUserData record);
}
