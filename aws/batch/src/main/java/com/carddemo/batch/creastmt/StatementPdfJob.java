package com.carddemo.batch.creastmt;

import com.carddemo.batch.core.CardDemoJob;
import com.carddemo.batch.core.JobFailure;
import com.carddemo.batch.core.JobOutcome;
import com.carddemo.batch.core.JobParams;
import com.carddemo.batch.core.JobRunner;
import com.carddemo.batch.core.ReturnCode;
import com.carddemo.batch.storage.ObjectStore;
import com.carddemo.batch.storage.S3Keys;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.util.LinkedHashMap;
import java.util.Map;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;
import org.apache.pdfbox.pdmodel.PDPageContentStream;
import org.apache.pdfbox.pdmodel.common.PDRectangle;
import org.apache.pdfbox.pdmodel.font.PDType1Font;
import org.apache.pdfbox.pdmodel.font.Standard14Fonts;
import org.springframework.stereotype.Component;

/**
 * {@code TXT2PDF1.JCL} (REXX {@code TXT2PDF}) → PDFBox rendering of {@code statement.txt} into
 * {@code statement.pdf} (Courier, one page per statement, continued on further pages when longer).
 * {@code --statementRunId} selects the {@code create-statements} run (default: this run's id).
 */
@Component
public class StatementPdfJob implements CardDemoJob {

    private static final float FONT_SIZE = 9f;
    private static final float LEADING = 11f;
    private static final float MARGIN = 40f;

    private final ObjectStore store;

    public StatementPdfJob(ObjectStore store) {
        this.store = store;
    }

    @Override
    public String name() {
        return "statement-pdf";
    }

    @Override
    public JobOutcome run(JobParams p) {
        String sourceRun = p.get("statementRunId").orElse(p.runId());
        if (!JobRunner.RUN_ID.matcher(sourceRun).matches()) {
            throw new JobFailure(ReturnCode.INPUT_ERROR, "--statementRunId must match " + JobRunner.RUN_ID);
        }
        String txtKey = S3Keys.statement(p.businessDate(), sourceRun, "txt");
        String text = new String(store.get(txtKey), StandardCharsets.UTF_8);
        int[] pages = {0};
        byte[] pdf = render(text, pages);
        String pdfKey = S3Keys.statement(p.businessDate(), sourceRun, "pdf");
        store.put(pdfKey, pdf, "application/pdf");
        Map<String, Object> counts = new LinkedHashMap<>();
        counts.put("pages", pages[0]);
        counts.put("source", store.uri(txtKey));
        counts.put("output", store.uri(pdfKey));
        return JobOutcome.ok(counts);
    }

    static byte[] render(String text, int[] pageCount) {
        PDType1Font font = new PDType1Font(Standard14Fonts.FontName.COURIER);
        PDRectangle size = PDRectangle.LETTER;
        int linesPerPage = (int) ((size.getHeight() - 2 * MARGIN) / LEADING);
        try (PDDocument doc = new PDDocument(); ByteArrayOutputStream out = new ByteArrayOutputStream()) {
            PDPageContentStream cs = null;
            int line = 0;
            for (String raw : text.split("\n")) {
                boolean statementStart = raw.contains("START OF STATEMENT");
                if (cs == null || line >= linesPerPage || (statementStart && line > 0)) {
                    if (cs != null) {
                        cs.endText();
                        cs.close();
                    }
                    PDPage page = new PDPage(size);
                    doc.addPage(page);
                    pageCount[0]++;
                    cs = new PDPageContentStream(doc, page);
                    cs.beginText();
                    cs.setFont(font, FONT_SIZE);
                    cs.setLeading(LEADING);
                    cs.newLineAtOffset(MARGIN, size.getHeight() - MARGIN);
                    line = 0;
                }
                cs.showText(raw.stripTrailing().replaceAll("[^\\x20-\\x7E]", "?"));
                cs.newLine();
                line++;
            }
            if (cs != null) {
                cs.endText();
                cs.close();
            } else {
                doc.addPage(new PDPage(size));
                pageCount[0]++;
            }
            doc.save(out);
            return out.toByteArray();
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }
}
