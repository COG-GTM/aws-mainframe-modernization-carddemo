package com.carddemo.online.config;

import com.carddemo.common.copybook.CardDataSeed;
import com.carddemo.common.copybook.CardRecord;
import com.carddemo.common.copybook.UserSecurityRecord;
import com.carddemo.common.copybook.UserSecuritySeed;
import com.carddemo.runtime.cics.SystemInfo;
import com.carddemo.runtime.vsam.FileControl;
import com.carddemo.runtime.vsam.JdbcKsdsStore;
import com.carddemo.runtime.vsam.KsdsFile;
import java.time.Clock;
import java.util.List;
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
        fct.register(seeded(new KsdsFile(CardDataSeed.FILE_NAME, CardRecord.LENGTH, CardRecord.KEY_OFFSET,
                CardRecord.KEY_LENGTH, new JdbcKsdsStore(dataSource, CardDataSeed.DSNAME)), CardDataSeed.records()));
        return fct;
    }

    private static KsdsFile seeded(KsdsFile file, List<String> records) {
        if (file.store().count() == 0) {
            file.load(records);
        }
        return file;
    }
}
