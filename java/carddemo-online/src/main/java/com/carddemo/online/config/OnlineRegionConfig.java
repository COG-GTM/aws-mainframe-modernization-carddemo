package com.carddemo.online.config;

import com.carddemo.common.copybook.AccountRecord;
import com.carddemo.common.copybook.AsciiSeedData;
import com.carddemo.common.copybook.CardXrefRecord;
import com.carddemo.common.copybook.CustomerRecord;
import com.carddemo.common.copybook.UserSecurityRecord;
import com.carddemo.common.copybook.UserSecuritySeed;
import com.carddemo.runtime.cics.SystemInfo;
import com.carddemo.runtime.vsam.FileControl;
import com.carddemo.runtime.vsam.JdbcKsdsStore;
import com.carddemo.runtime.vsam.KsdsFile;
import java.time.Clock;
import javax.sql.DataSource;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * The online "region": file control table (CSD DEFINE FILE entries) backed by the configured DataSource,
 * ASSIGN values and the clock. Files are seeded from the original datasets when empty. Later programs add
 * their files to {@link #fileControl} (see java/README.md).
 */
@Configuration
public class OnlineRegionConfig {

    @Bean
    @ConditionalOnMissingBean
    public Clock clock() {
        return Clock.systemDefaultZone();
    }

    @Bean
    public SystemInfo systemInfo(@Value("${carddemo.cics.applid:CARDDEMO}") String applid,
                                 @Value("${carddemo.cics.sysid:CICS}") String sysid) {
        return new SystemInfo(applid, sysid);
    }

    @Bean
    public FileControl fileControl(DataSource dataSource) {
        FileControl fct = new FileControl();
        KsdsFile usrsec = new KsdsFile(UserSecuritySeed.FILE_NAME, UserSecurityRecord.LENGTH,
                UserSecurityRecord.KEY_OFFSET, UserSecurityRecord.KEY_LENGTH,
                new JdbcKsdsStore(dataSource, "AWS.M2.CARDDEMO.USRSEC.VSAM.KSDS"));
        if (usrsec.store().count() == 0) {
            usrsec.load(UserSecuritySeed.records());
        }
        fct.register(usrsec);
        fct.register(ksds(dataSource, "ACCTDAT", "AWS.M2.CARDDEMO.ACCTDATA.VSAM.KSDS", AccountRecord.LENGTH, AccountRecord.KEY_OFFSET, AccountRecord.KEY_LENGTH, AsciiSeedData.ACCTDATA));
        fct.register(ksds(dataSource, "CUSTDAT", "AWS.M2.CARDDEMO.CUSTDATA.VSAM.KSDS", CustomerRecord.LENGTH, CustomerRecord.KEY_OFFSET, CustomerRecord.KEY_LENGTH, AsciiSeedData.CUSTDATA));
        KsdsFile ccxref = ksds(dataSource, "CCXREF", "AWS.M2.CARDDEMO.CARDXREF.VSAM.KSDS", CardXrefRecord.LENGTH, CardXrefRecord.KEY_OFFSET, CardXrefRecord.KEY_LENGTH, AsciiSeedData.CARDXREF);
        fct.register(ccxref);
        fct.register(ccxref.path("CXACAIX", CardXrefRecord.ACCT_AIX_OFFSET, CardXrefRecord.ACCT_AIX_LENGTH, false));
        return fct;
    }

    /** A CSD DEFINE FILE backed by table {@code dsname}, seeded from app/data/ASCII when empty. */
    private static KsdsFile ksds(DataSource dataSource, String file, String dsname, int lrecl, int keyOffset,
                                 int keyLength, String asciiSeed) {
        KsdsFile f = new KsdsFile(file, lrecl, keyOffset, keyLength, new JdbcKsdsStore(dataSource, dsname));
        if (f.store().count() == 0) {
            f.load(AsciiSeedData.records(asciiSeed, lrecl));
        }
        return f;
    }
}
