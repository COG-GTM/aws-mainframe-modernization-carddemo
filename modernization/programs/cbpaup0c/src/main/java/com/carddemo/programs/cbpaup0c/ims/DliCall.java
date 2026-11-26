package com.carddemo.programs.cbpaup0c.ims;

/** The EXEC DLI calls CBPAUP0C issues against PCB(PAUT-PCB-NUM) of PSB PSBPAUTB. */
public enum DliCall {
    /** EXEC DLI GN SEGMENT(PAUTSUM0). */
    GN_PAUTSUM0,
    /** EXEC DLI GNP SEGMENT(PAUTDTL1). */
    GNP_PAUTDTL1,
    /** EXEC DLI DLET SEGMENT(PAUTDTL1). */
    DLET_PAUTDTL1,
    /** EXEC DLI DLET SEGMENT(PAUTSUM0). */
    DLET_PAUTSUM0,
    /** EXEC DLI CHKP ID(...). */
    CHKP
}
