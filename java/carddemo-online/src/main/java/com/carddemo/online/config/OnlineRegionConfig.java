package com.carddemo.online.config;

import com.carddemo.common.copybook.AccountRecord;
import com.carddemo.common.copybook.CardDemoAsciiSeed;
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
        fct.register(seeded(dataSource, AccountRecord.FILE_NAME, AccountRecord.DSNAME, AccountRecord.LENGTH,
                AccountRecord.KEY_OFFSET, AccountRecord.KEY_LENGTH, AccountRecord.SEED_FILE));
        fct.register(seeded(dataSource, CustomerRecord.FILE_NAME, CustomerRecord.DSNAME, CustomerRecord.LENGTH,
                CustomerRecord.KEY_OFFSET, CustomerRecord.KEY_LENGTH, CustomerRecord.SEED_FILE));
        KsdsFile ccxref = seeded(dataSource, CardXrefRecord.FILE_NAME, CardXrefRecord.DSNAME, CardXrefRecord.LENGTH,
                CardXrefRecord.KEY_OFFSET, CardXrefRecord.KEY_LENGTH, CardXrefRecord.SEED_FILE);
        fct.register(ccxref);
        fct.register(ccxref.path(CardXrefRecord.ACCT_AIX_FILE_NAME, CardXrefRecord.ACCT_AIX_KEY_OFFSET,
                CardXrefRecord.ACCT_AIX_KEY_LENGTH, false));
        return fct;
    }

    /** A KSDS backed by the DataSource, loaded from app/data/ASCII when its table is empty. */
    static KsdsFile seeded(DataSource dataSource, String fileName, String dsname, int lrecl, int keyOffset,
                           int keyLength, String seedFile) {
        KsdsFile file = new KsdsFile(fileName, lrecl, keyOffset, keyLength, new JdbcKsdsStore(dataSource, dsname));
        if (file.store().count() == 0) {
            file.load(CardDemoAsciiSeed.records(seedFile, lrecl));
        }
        return file;
    }
}
