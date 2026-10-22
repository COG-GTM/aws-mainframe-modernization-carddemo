/**
 * CORPT00C - transaction report submission (transaction CR00, map CORPT0A / mapset CORPT00).
 * Builds the TRNRPT00 JCL (PROC TRANREPT with the date range) and writes it,
 * one 80-byte record at a time, to the extra-partition TDQ 'JOBS' (internal
 * reader). The TDQ write goes through an injectable job-submission sink.
 * Every SEND-TRNRPT-SCREEN ends the task (GO TO RETURN-TO-CICS).
 */
import {
  CCDA_MSG_INVALID_KEY,
  DFHAID,
  DFHCOLOR,
  DFHRESP,
  SymbolicMap,
  cobolString,
  isNumericText,
  moveAlnum,
  numvalC,
  populateHeaderInfo,
  type CicsContext,
  type CommandResult,
  type OnlineProgram,
} from '../framework/index.js';
import { BASE_COMMAREA_LENGTH, ProgramCommarea, isBlankField, moveNumeric } from './common.js';
import { callCsutldtc } from './cotrn02c.js';

const WS_PGMNAME = 'CORPT00C';
const WS_TRANID = 'CR00';
const WS_DATE_FORMAT = 'YYYY-MM-DD';
const JOBS_QUEUE = 'JOBS';

/**
 * EXEC CICS WRITEQ TD QUEUE('JOBS') FROM(JCL-RECORD) LENGTH(80) RESP(...):
 * receives each 80-byte JCL record; returns the RESP of the write.
 */
export type JobSubmissionSink = (ctx: CicsContext, queue: string, record: string) => CommandResult;

/** Default sink: the runtime's transient data queue (`OnlineRuntime.tdQueues.get('JOBS')`). */
export const tdqJobSink: JobSubmissionSink = (ctx, queue, record) => ctx.writeqTd(queue, record);

const line = (s: string): string => moveAlnum(s, 80);

/** JOB-DATA-1 with PARM-START-DATE-1/2 and PARM-END-DATE-1/2 filled in (17 x 80-byte JOB-LINES). */
export function buildJobLines(startDate: string, endDate: string): string[] {
  return [
    line("//TRNRPT00 JOB 'TRAN REPORT',CLASS=A,MSGCLASS=0,"),
    line('// NOTIFY=&SYSUID'),
    line('//*'),
    line("//JOBLIB JCLLIB ORDER=('AWS.M2.CARDDEMO.PROC')"),
    line('//*'),
    line('//STEP10 EXEC PROC=TRANREPT'),
    line('//*'),
    line('//STEP05R.SYMNAMES DD *'),
    line('TRAN-CARD-NUM,263,16,ZD'),
    line('TRAN-PROC-DT,305,10,CH'),
    "PARM-START-DATE,C'" + moveAlnum(startDate, 10) + moveAlnum("'", 52),
    "PARM-END-DATE,C'" + moveAlnum(endDate, 10) + moveAlnum("'", 54),
    line('/*'),
    line('//STEP10R.DATEPARM DD *'),
    moveAlnum(startDate, 10) + ' ' + moveAlnum(endDate, 10) + ' '.repeat(59),
    line('/*'),
    line('/*EOF'),
  ];
}

const p2 = (n: number): string => String(n).padStart(2, '0');
const p4 = (n: number): string => String(n).padStart(4, '0');

export function createCORPT00C(opts: { jobSink?: JobSubmissionSink } = {}): OnlineProgram {
  const jobSink = opts.jobSink ?? tdqJobSink;

  function run(ctx: CicsContext): void {
    const map = new SymbolicMap('CORPT0A', 'CORPT00');
    let commarea = ProgramCommarea.fresh(BASE_COMMAREA_LENGTH);
    let errFlg = false;
    let wsMessage = moveAlnum('', 80);
    let wsReportName = moveAlnum('', 10);
    let wsStartDate = '    -  -  ';
    let wsEndDate = '    -  -  ';
    let parmStartDate = moveAlnum('', 10);
    let parmEndDate = moveAlnum('', 10);

    const setMessage = (m: string): void => {
      wsMessage = moveAlnum(m, 80);
    };

    const sendTrnrptScreen = (): never => {
      populateHeaderInfo(map, { tranid: WS_TRANID, pgmname: WS_PGMNAME, now: ctx.now() });
      map.set('ERRMSG', wsMessage);
      ctx.sendMap(map, { mapset: 'CORPT00', erase: true, cursor: true });
      return ctx.return({ transid: WS_TRANID, commarea: commarea.toBuffer() });
    };

    const fail = (message: string, cursorField: string): never => {
      setMessage(message);
      errFlg = true;
      map.setLength(cursorField, -1);
      return sendTrnrptScreen();
    };

    const returnToPrevScreen = (): void => {
      ctx.xctl(commarea.prepareReturn(WS_TRANID, WS_PGMNAME), commarea.toBuffer());
    };

    const initializeAllFields = (): void => {
      map.setLength('MONTHLY', -1);
      for (const f of ['MONTHLY', 'YEARLY', 'CUSTOM', 'SDTMM', 'SDTDD', 'SDTYYYY', 'EDTMM', 'EDTDD', 'EDTYYYY', 'CONFIRM']) map.set(f, '');
      setMessage('');
    };

    const reportNameText = (): { value: string; delimitedBy: string } => ({ value: wsReportName, delimitedBy: ' ' });

    const wirteJobsubTdq = (jclRecord: string): void => {
      const r = ctx.track(jobSink(ctx, JOBS_QUEUE, jclRecord));
      if (r.resp !== DFHRESP.NORMAL) fail('Unable to Write TDQ (JOBS)...', 'MONTHLY');
    };

    const submitJobToIntrdr = (): void => {
      const confirm = map.get('CONFIRM');
      if (isBlankField(confirm)) {
        wsMessage = cobolString(['Please confirm to print the ', reportNameText(), ' report...'], wsMessage);
        errFlg = true;
        map.setLength('CONFIRM', -1);
        sendTrnrptScreen();
      }
      if (errFlg) return;
      if (confirm === 'Y' || confirm === 'y') {
        // CONTINUE
      } else if (confirm === 'N' || confirm === 'n') {
        initializeAllFields();
        errFlg = true;
        sendTrnrptScreen();
      } else {
        wsMessage = cobolString(['"', { value: confirm, delimitedBy: ' ' }, '" is not a valid value to confirm...'], wsMessage);
        errFlg = true;
        map.setLength('CONFIRM', -1);
        sendTrnrptScreen();
      }

      const jobLines = buildJobLines(parmStartDate, parmEndDate);
      let endLoop = false;
      for (let wsIdx = 1; !(wsIdx > 1000 || endLoop || errFlg); wsIdx++) {
        // JOB-LINES OCCURS 1000 redefines the 17-line JOB-DATA-1; the loop always stops at '/*EOF'.
        const jclRecord = jobLines[wsIdx - 1] ?? ' '.repeat(80);
        if (jclRecord === line('/*EOF') || isBlankField(jclRecord)) endLoop = true;
        wirteJobsubTdq(jclRecord);
      }
    };

    const checkDate = (date: string, message: string, cursorField: string): void => {
      const r = callCsutldtc(date, WS_DATE_FORMAT);
      if (r.sevCd !== '0000' && r.msgNum !== '2513') fail(message, cursorField);
    };

    const processEnterKey = (): void => {
      if (!isBlankField(map.get('MONTHLY'))) {
        wsReportName = moveAlnum('Monthly', 10);
        const now = ctx.now();
        const year = now.getFullYear();
        const month = now.getMonth() + 1;
        wsStartDate = `${p4(year)}-${p2(month)}-01`;
        parmStartDate = wsStartDate;
        // first day of next month - 1 (FUNCTION DATE-OF-INTEGER(INTEGER-OF-DATE(...) - 1))
        const last = new Date(Date.UTC(year, month, 0));
        wsEndDate = `${p4(last.getUTCFullYear())}-${p2(last.getUTCMonth() + 1)}-${p2(last.getUTCDate())}`;
        parmEndDate = wsEndDate;
        submitJobToIntrdr();
      } else if (!isBlankField(map.get('YEARLY'))) {
        wsReportName = moveAlnum('Yearly', 10);
        const year = p4(ctx.now().getFullYear());
        wsStartDate = `${year}-01-01`;
        parmStartDate = wsStartDate;
        wsEndDate = `${year}-12-31`;
        parmEndDate = wsEndDate;
        submitJobToIntrdr();
      } else if (!isBlankField(map.get('CUSTOM'))) {
        const empties: [string, string][] = [
          ['SDTMM', 'Start Date - Month can NOT be empty...'],
          ['SDTDD', 'Start Date - Day can NOT be empty...'],
          ['SDTYYYY', 'Start Date - Year can NOT be empty...'],
          ['EDTMM', 'End Date - Month can NOT be empty...'],
          ['EDTDD', 'End Date - Day can NOT be empty...'],
          ['EDTYYYY', 'End Date - Year can NOT be empty...'],
        ];
        for (const [f, msg] of empties) if (isBlankField(map.get(f))) fail(msg, f);

        // COMPUTE WS-NUM-99 / WS-NUM-9999 = FUNCTION NUMVAL-C(...) and MOVE back to the screen field.
        for (const f of ['SDTMM', 'SDTDD', 'EDTMM', 'EDTDD']) map.set(f, moveNumeric(numvalC(map.get(f)), 2, 0, false).toFixed(0).padStart(2, '0'));
        for (const f of ['SDTYYYY', 'EDTYYYY']) map.set(f, moveNumeric(numvalC(map.get(f)), 4, 0, false).toFixed(0).padStart(4, '0'));

        if (!isNumericText(map.get('SDTMM')) || map.get('SDTMM') > '12') fail('Start Date - Not a valid Month...', 'SDTMM');
        if (!isNumericText(map.get('SDTDD')) || map.get('SDTDD') > '31') fail('Start Date - Not a valid Day...', 'SDTDD');
        if (!isNumericText(map.get('SDTYYYY'))) fail('Start Date - Not a valid Year...', 'SDTYYYY');
        if (!isNumericText(map.get('EDTMM')) || map.get('EDTMM') > '12') fail('End Date - Not a valid Month...', 'EDTMM');
        if (!isNumericText(map.get('EDTDD')) || map.get('EDTDD') > '31') fail('End Date - Not a valid Day...', 'EDTDD');
        if (!isNumericText(map.get('EDTYYYY'))) fail('End Date - Not a valid Year...', 'EDTYYYY');

        wsStartDate = `${map.get('SDTYYYY')}-${map.get('SDTMM')}-${map.get('SDTDD')}`;
        wsEndDate = `${map.get('EDTYYYY')}-${map.get('EDTMM')}-${map.get('EDTDD')}`;
        checkDate(wsStartDate, 'Start Date - Not a valid date...', 'SDTMM');
        checkDate(wsEndDate, 'End Date - Not a valid date...', 'EDTMM');

        parmStartDate = wsStartDate;
        parmEndDate = wsEndDate;
        wsReportName = moveAlnum('Custom', 10);
        if (!errFlg) submitJobToIntrdr();
      } else {
        fail('Select a report type to print report...', 'MONTHLY');
      }

      if (!errFlg) {
        initializeAllFields();
        map.setColor('ERRMSG', DFHCOLOR.DFHGREEN);
        wsMessage = cobolString([reportNameText(), ' report submitted for printing ...'], wsMessage);
        map.setLength('MONTHLY', -1);
        sendTrnrptScreen();
      }
    };

    // MAIN-PARA
    map.set('ERRMSG', '');
    if (ctx.eib.eibcalen === 0) {
      commarea.toProgram = 'COSGN00C';
      returnToPrevScreen();
    } else {
      commarea = ProgramCommarea.fromDfhCommarea(ctx.commarea, BASE_COMMAREA_LENGTH);
      if (!commarea.isPgmReenter) {
        commarea.setPgmReenter();
        map.lowValues();
        map.setLength('MONTHLY', -1);
        sendTrnrptScreen();
      } else {
        ctx.receiveMap(map);
        switch (ctx.eib.eibaid) {
          case DFHAID.DFHENTER:
            processEnterKey();
            break;
          case DFHAID.DFHPF3:
            commarea.toProgram = 'COMEN01C';
            returnToPrevScreen();
            break;
          default:
            errFlg = true;
            map.setLength('MONTHLY', -1);
            setMessage(CCDA_MSG_INVALID_KEY);
            sendTrnrptScreen();
        }
      }
    }
    ctx.return({ transid: WS_TRANID, commarea: commarea.toBuffer() });
  }

  return { program: WS_PGMNAME, transid: WS_TRANID, run };
}

export const CORPT00C: OnlineProgram = createCORPT00C();
