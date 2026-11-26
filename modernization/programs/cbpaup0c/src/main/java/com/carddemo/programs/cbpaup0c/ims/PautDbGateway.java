package com.carddemo.programs.cbpaup0c.ims;

import com.carddemo.programs.cbpaup0c.copybook.PendingAuthDetails;
import com.carddemo.programs.cbpaup0c.copybook.PendingAuthSummary;

/**
 * DL/I access to the pending-authorization database DBPAUTP0 (HIDAM: root PAUTSUM0, child PAUTDTL1) through
 * PSB PSBPAUTB, PROCOPT=AP. Every method returns the two-character DIBSTAT exactly as IMS would set it.
 */
public interface PautDbGateway {

    /** GN SEGMENT(PAUTSUM0) INTO(PENDING-AUTH-SUMMARY): next root in key sequence; 'GB' at end of database. */
    DliResult<PendingAuthSummary> getNextSummary();

    /** GNP SEGMENT(PAUTDTL1) INTO(PENDING-AUTH-DETAILS): next child of the current root; 'GE' when none left. */
    DliResult<PendingAuthDetails> getNextDetailWithinParent();

    /** DLET SEGMENT(PAUTDTL1) FROM(PENDING-AUTH-DETAILS): deletes the detail last retrieved. */
    String deleteDetail(PendingAuthDetails detail);

    /** DLET SEGMENT(PAUTSUM0) FROM(PENDING-AUTH-SUMMARY): deletes the current root and all its dependents. */
    String deleteSummary(PendingAuthSummary summary);

    /** CHKP ID(checkpointId): basic checkpoint, commits the unit of work. */
    String checkpoint(String checkpointId);
}
