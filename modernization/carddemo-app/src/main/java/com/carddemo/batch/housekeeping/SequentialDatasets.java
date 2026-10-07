package com.carddemo.batch.housekeeping;

import com.carddemo.batch.BatchOutputProperties;
import com.carddemo.batch.DatedOutputFiles;
import com.carddemo.batch.harness.BatchCommandLine;
import com.carddemo.batch.harness.DdParameters;
import com.carddemo.batch.harness.ReturnCode;
import com.carddemo.batch.harness.ReturnCodeException;
import com.carddemo.batch.harness.Sysout;
import com.carddemo.batch.load.VsamDatasetLoader.Dataset;
import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.codec.RecordLayout;
import com.carddemo.common.file.FileStatus;
import com.carddemo.common.file.FileStatusException;
import com.carddemo.common.file.RecordFiles;
import com.carddemo.transaction.TranCatBalanceRecord;
import com.carddemo.transaction.TranCatBalanceRepository;
import com.carddemo.transaction.TransactionRecord;
import com.carddemo.transaction.TransactionRepository;
import java.nio.file.Path;
import java.util.List;
import java.util.Locale;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.StepExecution;
import org.springframework.stereotype.Component;

/**
 * DD resolution shared by the housekeeping and report jobs (ADR-0012, ADR-0015). A KSDS DD is its table unless
 * {@code --<DD>=<path>} names an unload file; a GDG DD is generation {@code (0)} (input) or {@code (+1)} (output) of
 * its base in {@code batch_output_file} unless {@code --<DD>=<path>} names a file. Generations are fixed-length
 * records; explicit files use the run format of every other file DD (line sequential for ASCII, fixed for EBCDIC).
 */
@Component
public class SequentialDatasets {

    public static final String SYSOUT_CONTEXT_KEY = "SYSOUT";
    public static final String OUTPUT_CONTEXT_KEY = "OUTPUT";

    private final DatedOutputFiles generations;
    private final BatchOutputProperties output;
    private final TransactionRepository transactions;
    private final TranCatBalanceRepository balances;

    public SequentialDatasets(DatedOutputFiles generations, BatchOutputProperties output,
                              TransactionRepository transactions, TranCatBalanceRepository balances) {
        this.generations = generations;
        this.output = output;
        this.transactions = transactions;
        this.balances = balances;
    }

    /** The KSDS records in key order: its table, or the unload file named by {@code --<dd>=<path>}. */
    public List<FixedWidthRecord> ksds(Dataset dataset, JobParameters parameters, String dd,
                                       RecordEncoding encoding) {
        if (!DdParameters.isTable(parameters, dd)) {
            return dataset.read(DdParameters.path(parameters, dd), encoding);
        }
        return switch (dataset) {
            case TRANSACT -> transactions.findAllByOrderByTranIdAsc().stream()
                    .map(t -> TransactionRecord.MAPPER.toRecord(t.toRecord(), encoding)).toList();
            case TCATBALF -> balances.findAllInKeyOrder().stream()
                    .map(b -> TranCatBalanceRecord.MAPPER.toRecord(b.toRecord(), encoding)).toList();
            default -> throw new ReturnCodeException(ReturnCode.TERMINAL, dataset + " is not a housekeeping dataset");
        };
    }

    /** A sequential input: {@code --<dd>=<path>}, else generation {@code (0)} of {@code gdgBase}. */
    public List<FixedWidthRecord> sequential(JobParameters parameters, String dd, String gdgBase, RecordLayout layout,
                                             RecordEncoding encoding) {
        Path path = inputPath(parameters, dd, gdgBase);
        if (!DdParameters.isTable(parameters, dd) && encoding != RecordEncoding.EBCDIC) {
            return RecordFiles.readLines(dd, path, layout, encoding);
        }
        return RecordFiles.readFixed(dd, path, layout, encoding);
    }

    public Path inputPath(JobParameters parameters, String dd, String gdgBase) {
        if (!DdParameters.isTable(parameters, dd)) {
            return DdParameters.path(parameters, dd);
        }
        return generations.generation(gdgBase, 0)
                .orElseThrow(() -> new FileStatusException(dd, "OPEN", FileStatus.FILE_NOT_FOUND));
    }

    /** Writes {@code --<dd>=<path>}, else catalogues generation {@code (+1)} of {@code gdgBase}; returns the file. */
    public String write(StepExecution step, String dd, String gdgBase, List<FixedWidthRecord> records,
                        RecordEncoding encoding) {
        JobParameters parameters = step.getJobParameters();
        String file;
        if (!DdParameters.isTable(parameters, dd)) {
            Path path = DdParameters.path(parameters, dd).toAbsolutePath().normalize();
            if (encoding == RecordEncoding.EBCDIC) {
                RecordFiles.writeFixed(dd, path, records);
            } else {
                RecordFiles.writeLines(dd, path, records, false);
            }
            file = path.toString();
        } else {
            file = generations.write(gdgBase, parameters.getLocalDate(BatchCommandLine.RUN_DATE),
                    step.getJobExecutionId(), records).getFilePath();
        }
        step.getExecutionContext().putString(OUTPUT_CONTEXT_KEY + "." + dd, file);
        return file;
    }

    /** The step's SYSOUT ({@code --SYSOUT=<path>} or the default under the output directory). */
    public Sysout sysout(StepExecution step, String jobName) {
        Path path = DdParameters.sysout(step.getJobParameters(), output.outputDir(), jobName,
                step.getJobExecutionId());
        step.getExecutionContext().putString(SYSOUT_CONTEXT_KEY, path.toAbsolutePath().normalize().toString());
        return Sysout.open(path);
    }

    /** The cluster name of a KSDS ({@code AWS.M2.CARDDEMO.<dataset>.VSAM.KSDS}). */
    public static String cluster(Dataset dataset) {
        return "AWS.M2.CARDDEMO." + dataset.name().toUpperCase(Locale.ROOT) + ".VSAM.KSDS";
    }
}
