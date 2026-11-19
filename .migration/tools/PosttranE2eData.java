// POSTTRAN end-to-end seed/unload through the existing KsdsDataService (D-021); no master-loading CLI exists (D-053).
// Run against the built jar's classes (Java 21 single-file source launch):
//   java -cp "<x>/BOOT-INF/classes:<x>/BOOT-INF/lib/*" .migration/tools/PosttranE2eData.java load   <masters-dir> <stale-record-file> --spring.datasource.url=...
//   java -cp ... PosttranE2eData.java unload <out-dir> --spring.datasource.url=...
// load: empties the four tables, loads {acctdata,cardxref,tcatbal}.txt and one stale TRANSACT row (first line of the file,
//       TRAN-ID replaced by STALE-E2E-000001) that a new POSTTRAN instance must remove (D-024).
// unload: writes ACCTDATA.unl, TCATBALF.unl, TRANSACT.unl in KSDS key order, line sequential like the GnuCOBOL unloader.
import com.carddemo.app.CardDemoApplication;
import com.carddemo.app.data.KsdsDataService;
import com.carddemo.app.data.KsdsDataService.Ksds;
import com.carddemo.common.cobol.CobolCharsets;
import com.carddemo.common.record.RecordFiles;
import org.springframework.boot.SpringApplication;
import org.springframework.context.ConfigurableApplicationContext;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;

public class PosttranE2eData {
    public static void main(String[] args) throws Exception {
        String mode = args[0];
        Path dir = Path.of(args[1]);
        int boot = mode.equals("load") ? 3 : 2;
        try (ConfigurableApplicationContext ctx = new SpringApplication(CardDemoApplication.class)
                .run(Arrays.copyOfRange(args, boot, args.length))) {
            KsdsDataService data = ctx.getBean(KsdsDataService.class);
            if (mode.equals("load")) {
                data.deleteAll();
                data.loadSampleData(dir);
                String first = RecordFiles.readLines(Path.of(args[2]), 350, CobolCharsets.ASCII).get(0);
                data.load(Ksds.TRANSACT, List.of("STALE-E2E-000001" + first.substring(16)));
            } else {
                Files.createDirectories(dir);
                for (Ksds k : List.of(Ksds.ACCTDATA, Ksds.TCATBALF, Ksds.TRANSACT)) {
                    RecordFiles.writeLines(dir.resolve(k + ".unl"), data.unload(k), CobolCharsets.ASCII);
                }
            }
        }
    }
}
