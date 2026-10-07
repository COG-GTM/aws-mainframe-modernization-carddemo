package com.carddemo.web;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.header;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.carddemo.batch.report.ReportExecution;
import com.carddemo.batch.report.ReportName;
import com.carddemo.batch.report.ReportQueueFullException;
import com.carddemo.batch.report.ReportStatus;
import com.carddemo.batch.report.ReportWindow;
import com.carddemo.user.UserType;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.time.LocalDate;
import java.time.OffsetDateTime;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.RejectedExecutionException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.ResultActions;

/** {@code docs/modernization/rules/CORPT00C.md} against {@code POST/GET /api/v1/reports/transactions}. */
class TransactionReportRulesTest extends OnlineWebTest {

    static final String REPORTS = "/api/v1/reports/transactions";

    private String user() {
        return bearer("USER0001", UserType.USER);
    }

    private String admin() {
        return bearer("ADMIN001", UserType.ADMIN);
    }

    static ReportExecution execution(long id, ReportWindow window, String by, ReportStatus status) {
        return new ReportExecution(id, "tranrept", window.name(), window.startDate(), window.endDate(),
                LocalDate.of(2022, 7, 6), "EBCDIC", by, status, null, List.of(), null, null, null,
                OffsetDateTime.parse("2022-07-06T13:45:10Z"), null, null);
    }

    @BeforeEach
    void givenAQueue() {
        given(reportLauncher.submit(any(ReportWindow.class), anyString()))
                .willAnswer(i -> execution(7, i.getArgument(0), i.getArgument(1), ReportStatus.QUEUED));
    }

    private ObjectNode request(String type, String confirm) {
        return json.createObjectNode().put("reportType", type).put("confirm", confirm);
    }

    private ObjectNode custom(String sm, String sd, String sy, String em, String ed, String ey, String confirm) {
        ObjectNode body = request("Custom", confirm);
        body.putObject("startDate").put("month", sm).put("day", sd).put("year", sy);
        body.putObject("endDate").put("month", em).put("day", ed).put("year", ey);
        return body;
    }

    private ResultActions submit(ObjectNode body) throws Exception {
        return mvc.perform(post(REPORTS).header(HttpHeaders.AUTHORIZATION, user())
                .contentType(MediaType.APPLICATION_JSON).content(json.writeValueAsString(body)));
    }

    private static void rejected(ResultActions result, String field, String message) throws Exception {
        result.andExpect(status().isBadRequest()).andExpect(jsonPath("$.code").value("INVREQ"))
                .andExpect(jsonPath("$.field").value(field)).andExpect(jsonPath("$.message").value(message));
    }

    private ReportWindow submitted() {
        ArgumentCaptor<ReportWindow> window = ArgumentCaptor.forClass(ReportWindow.class);
        verify(reportLauncher).submit(window.capture(), eq("USER0001"));
        return window.getValue();
    }

    @Test
    void R1_noSessionReturnsToSignOn() throws Exception {
        mvc.perform(post(REPORTS).contentType(MediaType.APPLICATION_JSON).content("{}"))
                .andExpect(status().isUnauthorized()).andExpect(jsonPath("$.toProgram").value("COSGN00C"));
    }

    @Test
    void R2_nothingIsSubmittedBeforeConfirmation() throws Exception {
        submit(request("Monthly", "")).andExpect(status().isOk());
        verify(reportLauncher, never()).submit(any(), anyString());
    }

    @Test
    void R3_enterProcessesTheRequest() throws Exception {
        submit(request("Yearly", "")).andExpect(status().isOk()).andExpect(jsonPath("$.state").value("VALIDATED"));
    }

    @Test
    void R4_pf3AlwaysReturnsToTheMainMenu() throws Exception {
        submit(request("Monthly", "")).andExpect(jsonPath("$.exit.toProgram").value("COMEN01C"))
                .andExpect(jsonPath("$.exit.toTranId").value("CM00"));
    }

    @Test
    void R5_otherKeysAreInvalid() throws Exception {
        mvc.perform(put(REPORTS).header(HttpHeaders.AUTHORIZATION, user()))
                .andExpect(status().isMethodNotAllowed()).andExpect(jsonPath("$.code").value("INVALID_KEY"));
    }

    @Test
    void R6_monthlyIsTheCurrentCalendarMonth() throws Exception {
        submit(request("Monthly", "Y")).andExpect(status().isAccepted())
                .andExpect(jsonPath("$.reportName").value("Monthly"))
                .andExpect(jsonPath("$.parmStartDate").value("2022-07-01"))
                .andExpect(jsonPath("$.parmEndDate").value("2022-07-31"));
        ReportWindow window = submitted();
        assertThat(window.startDate()).isEqualTo(LocalDate.of(2022, 7, 1));
        assertThat(window.endDate()).isEqualTo(LocalDate.of(2022, 7, 31));
    }

    @Test
    void R7_yearlyIsTheCurrentCalendarYear() throws Exception {
        submit(request("y", "")).andExpect(jsonPath("$.reportName").value("Yearly"))
                .andExpect(jsonPath("$.parmStartDate").value("2022-01-01"))
                .andExpect(jsonPath("$.parmEndDate").value("2022-12-31"));
    }

    @Test
    void R8_customDatesAreRequiredInScreenOrder() throws Exception {
        rejected(submit(custom("", "", "", "", "", "", "")), "startDate.month",
                "Start Date - Month can NOT be empty...");
        rejected(submit(custom("1", "", "", "", "", "", "")), "startDate.day", "Start Date - Day can NOT be empty...");
        rejected(submit(custom("1", "1", " ", "", "", "", "")), "startDate.year",
                "Start Date - Year can NOT be empty...");
        rejected(submit(custom("1", "1", "2022", "", "", "", "")), "endDate.month",
                "End Date - Month can NOT be empty...");
        rejected(submit(custom("1", "1", "2022", "7", "", "", "")), "endDate.day",
                "End Date - Day can NOT be empty...");
        rejected(submit(custom("1", "1", "2022", "7", "6", "", "")), "endDate.year",
                "End Date - Year can NOT be empty...");
        rejected(submit(request("Custom", "")), "startDate.month", "Start Date - Month can NOT be empty...");
    }

    @Test
    void R9_fieldsAreNormalisedThroughNumvalC() throws Exception {
        submit(custom("1", " 2", "2022", "7", "6", "2022", "")).andExpect(status().isOk())
                .andExpect(jsonPath("$.startDate.month").value("01"))
                .andExpect(jsonPath("$.startDate.day").value("02"))
                .andExpect(jsonPath("$.endDate.month").value("07"));
        rejected(submit(custom("ab", "01", "2022", "07", "06", "2022", "")), "startDate",
                "Start Date - Not a valid date...");
    }

    @Test
    void R10_monthDayAndYearRanges() throws Exception {
        rejected(submit(custom("13", "01", "2022", "07", "06", "2022", "")), "startDate.month",
                "Start Date - Not a valid Month...");
        rejected(submit(custom("12", "32", "2022", "07", "06", "2022", "")), "startDate.day",
                "Start Date - Not a valid Day...");
        rejected(submit(custom("01", "01", "2022", "13", "06", "2022", "")), "endDate.month",
                "End Date - Not a valid Month...");
        rejected(submit(custom("01", "01", "2022", "07", "99", "2022", "")), "endDate.day",
                "End Date - Not a valid Day...");
    }

    @Test
    void R11_calendarCheckThroughCsutldtc() throws Exception {
        rejected(submit(custom("02", "30", "2022", "07", "06", "2022", "")), "startDate",
                "Start Date - Not a valid date...");
        rejected(submit(custom("01", "01", "2022", "04", "31", "2022", "")), "endDate",
                "End Date - Not a valid date...");
        rejected(submit(custom("02", "29", "2023", "03", "01", "2023", "")), "startDate",
                "Start Date - Not a valid date...");
        submit(custom("02", "29", "2024", "03", "01", "2024", "")).andExpect(status().isOk());
    }

    @Test
    void reversedRangeIsRejected() throws Exception {
        rejected(submit(custom("07", "06", "2022", "01", "01", "2022", "Y")), "startDate",
                "Start Date can NOT be after End Date...");
        verify(reportLauncher, never()).submit(any(), anyString());
        submit(custom("07", "06", "2022", "07", "06", "2022", "")).andExpect(status().isOk());
    }

    @Test
    void R12_customPassesTheTypedRange() throws Exception {
        submit(custom("01", "01", "2022", "07", "06", "2022", "Y")).andExpect(status().isAccepted())
                .andExpect(jsonPath("$.reportName").value("Custom"));
        ReportWindow window = submitted();
        assertThat(window.name()).isEqualTo(ReportName.CUSTOM);
        assertThat(window.startDate()).isEqualTo(LocalDate.of(2022, 1, 1));
        assertThat(window.endDate()).isEqualTo(LocalDate.of(2022, 7, 6));
    }

    @Test
    void R13_noSelectorAsksForAReportType() throws Exception {
        rejected(submit(request("", "Y")), "reportType", "Select a report type to print report...");
        rejected(submit(request("Weekly", "")), "reportType", "Report type must be Monthly, Yearly or Custom...");
    }

    @Test
    void R14_aSubmissionAnswersWithTheExecution() throws Exception {
        submit(custom("01", "01", "2022", "07", "06", "2022", "Y")).andExpect(status().isAccepted())
                .andExpect(header().string(HttpHeaders.LOCATION, REPORTS + "/7"))
                .andExpect(jsonPath("$.state").value("SUBMITTED"))
                .andExpect(jsonPath("$.executionId").value(7))
                .andExpect(jsonPath("$.status").value("QUEUED"))
                .andExpect(jsonPath("$.statusUrl").value(REPORTS + "/7"))
                .andExpect(jsonPath("$.message").value("Custom report submitted for printing ..."));
    }

    @Test
    void R15_blankConfirmAsksForConfirmation() throws Exception {
        submit(request("Monthly", " ")).andExpect(jsonPath("$.state").value("VALIDATED"))
                .andExpect(jsonPath("$.message").value("Please confirm to print the Monthly report..."))
                .andExpect(jsonPath("$.executionId").doesNotExist());
    }

    @Test
    void R16_lowerCaseYConfirms() throws Exception {
        submit(request("Yearly", "y")).andExpect(status().isAccepted());
        assertThat(submitted().name()).isEqualTo(ReportName.YEARLY);
    }

    @Test
    void R17_nClearsAndOtherValuesAreInvalid() throws Exception {
        submit(request("Monthly", "n")).andExpect(status().isOk()).andExpect(jsonPath("$.state").value("CANCELLED"))
                .andExpect(jsonPath("$.message").value("")).andExpect(jsonPath("$.reportName").doesNotExist());
        rejected(submit(request("Monthly", "X")), "confirm", "\"X\" is not a valid value to confirm...");
        verify(reportLauncher, never()).submit(any(), anyString());
    }

    @Test
    void R18_confirmedRequestsQueueTheTranreptStream() throws Exception {
        submit(request("Monthly", "Y")).andExpect(status().isAccepted());
        verify(reportLauncher).submit(any(ReportWindow.class), eq("USER0001"));
    }

    @Test
    void R19_aFullQueueCannotWriteTheTdq() throws Exception {
        given(reportLauncher.submit(any(ReportWindow.class), anyString())).willThrow(new ReportQueueFullException(
                9, "Unable to Write TDQ (JOBS)...", new RejectedExecutionException("full")));
        submit(request("Monthly", "Y")).andExpect(status().isServiceUnavailable())
                .andExpect(jsonPath("$.code").value("NOSPACE"))
                .andExpect(jsonPath("$.message").value("Unable to Write TDQ (JOBS)..."))
                .andExpect(jsonPath("$.executionId").value(9));
    }

    @Test
    void R20_returnCarriesTheFromFields() throws Exception {
        submit(request("Monthly", "")).andExpect(jsonPath("$.exit.fromTranId").value("CR00"))
                .andExpect(jsonPath("$.exit.fromProgram").value("CORPT00C"));
    }

    @Test
    void R21_theScreenHeaderNamesCr00() throws Exception {
        submit(request("Monthly", "")).andExpect(jsonPath("$.header.tranId").value("CR00"))
                .andExpect(jsonPath("$.header.programName").value("CORPT00C"));
    }

    @Test
    void pollingShowsOnlyYourOwnExecutionsUnlessAdmin() throws Exception {
        ReportWindow window = new ReportWindow(ReportName.MONTHLY, LocalDate.of(2022, 7, 1),
                LocalDate.of(2022, 7, 31), null, null);
        given(reportLauncher.find(anyLong())).willReturn(Optional.empty());
        given(reportLauncher.find(5)).willReturn(Optional.of(execution(5, window, "USER0002", ReportStatus.RUNNING)));
        given(reportLauncher.jobRuns(any())).willReturn(List.of());
        mvc.perform(get(REPORTS + "/5").header(HttpHeaders.AUTHORIZATION, user()))
                .andExpect(status().isNotFound()).andExpect(jsonPath("$.code").value("NOTFND"));
        mvc.perform(get(REPORTS + "/5").header(HttpHeaders.AUTHORIZATION, admin()))
                .andExpect(status().isOk()).andExpect(jsonPath("$.status").value("RUNNING"))
                .andExpect(jsonPath("$.report").doesNotExist());
        mvc.perform(get(REPORTS + "/5/report").header(HttpHeaders.AUTHORIZATION, admin()))
                .andExpect(status().isNotFound())
                .andExpect(jsonPath("$.message").value("Report NOT available: execution is RUNNING..."));
        mvc.perform(get(REPORTS + "/99").header(HttpHeaders.AUTHORIZATION, admin()))
                .andExpect(status().isNotFound()).andExpect(jsonPath("$.message").value("Report execution NOT found..."));
    }
}
