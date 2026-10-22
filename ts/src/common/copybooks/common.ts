/**
 * Common working-storage / linkage copybooks: COCOM01Y, CSUSR01Y, CSDAT01Y,
 * CSMSG01Y, CSMSG02Y, CODATECN, CVCRD01Y, COTTL01Y, COADM02Y, COMEN02Y,
 * UNUSED1Y, CSUTLDWY.
 */
import { alnum, defineRecord, group, num, occurs, redefines, type RecordOf } from '../record/layout.js';

/** COCOM01Y CARDDEMO-COMMAREA (160 bytes): the CICS COMMAREA passed between online programs. */
export const CarddemoCommareaLayout = defineRecord('CARDDEMO-COMMAREA', {
  cdemoGeneralInfo: group('CDEMO-GENERAL-INFO', {
    cdemoFromTranid: alnum('CDEMO-FROM-TRANID', 'X(04)'),
    cdemoFromProgram: alnum('CDEMO-FROM-PROGRAM', 'X(08)'),
    cdemoToTranid: alnum('CDEMO-TO-TRANID', 'X(04)'),
    cdemoToProgram: alnum('CDEMO-TO-PROGRAM', 'X(08)'),
    cdemoUserId: alnum('CDEMO-USER-ID', 'X(08)'),
    cdemoUserType: alnum('CDEMO-USER-TYPE', 'X(01)'),
    cdemoPgmContext: num('CDEMO-PGM-CONTEXT', '9(01)'),
  }),
  cdemoCustomerInfo: group('CDEMO-CUSTOMER-INFO', {
    cdemoCustId: num('CDEMO-CUST-ID', '9(09)'),
    cdemoCustFname: alnum('CDEMO-CUST-FNAME', 'X(25)'),
    cdemoCustMname: alnum('CDEMO-CUST-MNAME', 'X(25)'),
    cdemoCustLname: alnum('CDEMO-CUST-LNAME', 'X(25)'),
  }),
  cdemoAccountInfo: group('CDEMO-ACCOUNT-INFO', {
    cdemoAcctId: num('CDEMO-ACCT-ID', '9(11)'),
    cdemoAcctStatus: alnum('CDEMO-ACCT-STATUS', 'X(01)'),
  }),
  cdemoCardInfo: group('CDEMO-CARD-INFO', {
    cdemoCardNum: num('CDEMO-CARD-NUM', '9(16)'),
  }),
  cdemoMoreInfo: group('CDEMO-MORE-INFO', {
    cdemoLastMap: alnum('CDEMO-LAST-MAP', 'X(7)'),
    cdemoLastMapset: alnum('CDEMO-LAST-MAPSET', 'X(7)'),
  }),
});
export type CarddemoCommarea = RecordOf<typeof CarddemoCommareaLayout.root.fields>;
/** 88-levels of COCOM01Y. */
export const CDEMO_USRTYP_ADMIN = 'A';
export const CDEMO_USRTYP_USER = 'U';
export const CDEMO_PGM_ENTER = 0;
export const CDEMO_PGM_REENTER = 1;

/** CSUSR01Y SEC-USER-DATA (80 bytes): USRSEC file record. */
export const SecUserDataLayout = defineRecord('SEC-USER-DATA', {
  secUsrId: alnum('SEC-USR-ID', 'X(08)'),
  secUsrFname: alnum('SEC-USR-FNAME', 'X(20)'),
  secUsrLname: alnum('SEC-USR-LNAME', 'X(20)'),
  secUsrPwd: alnum('SEC-USR-PWD', 'X(08)'),
  secUsrType: alnum('SEC-USR-TYPE', 'X(01)'),
  secUsrFiller: alnum('SEC-USR-FILLER', 'X(23)'),
});
export type SecUserData = RecordOf<typeof SecUserDataLayout.root.fields>;

/** CSDAT01Y WS-DATE-TIME. */
export const WsDateTimeLayout = defineRecord('WS-DATE-TIME', {
  wsCurdateData: group('WS-CURDATE-DATA', {
    wsCurdate: group('WS-CURDATE', {
      wsCurdateYear: num('WS-CURDATE-YEAR', '9(04)'),
      wsCurdateMonth: num('WS-CURDATE-MONTH', '9(02)'),
      wsCurdateDay: num('WS-CURDATE-DAY', '9(02)'),
    }),
    wsCurdateN: redefines('wsCurdate', num('WS-CURDATE-N', '9(08)')),
    wsCurtime: group('WS-CURTIME', {
      wsCurtimeHours: num('WS-CURTIME-HOURS', '9(02)'),
      wsCurtimeMinute: num('WS-CURTIME-MINUTE', '9(02)'),
      wsCurtimeSecond: num('WS-CURTIME-SECOND', '9(02)'),
      wsCurtimeMilsec: num('WS-CURTIME-MILSEC', '9(02)'),
    }),
    wsCurtimeN: redefines('wsCurtime', num('WS-CURTIME-N', '9(08)')),
  }),
  wsCurdateMmDdYy: group('WS-CURDATE-MM-DD-YY', {
    wsCurdateMm: num('WS-CURDATE-MM', '9(02)'),
    filler: alnum('FILLER', 'X(01)', '/'),
    wsCurdateDd: num('WS-CURDATE-DD', '9(02)'),
    filler2: alnum('FILLER', 'X(01)', '/'),
    wsCurdateYy: num('WS-CURDATE-YY', '9(02)'),
  }),
  wsCurtimeHhMmSs: group('WS-CURTIME-HH-MM-SS', {
    wsCurtimeHh: num('WS-CURTIME-HH', '9(02)'),
    filler: alnum('FILLER', 'X(01)', ':'),
    wsCurtimeMm: num('WS-CURTIME-MM', '9(02)'),
    filler2: alnum('FILLER', 'X(01)', ':'),
    wsCurtimeSs: num('WS-CURTIME-SS', '9(02)'),
  }),
  wsTimestamp: group('WS-TIMESTAMP', {
    wsTimestampDtYyyy: num('WS-TIMESTAMP-DT-YYYY', '9(04)'),
    filler: alnum('FILLER', 'X(01)', '-'),
    wsTimestampDtMm: num('WS-TIMESTAMP-DT-MM', '9(02)'),
    filler2: alnum('FILLER', 'X(01)', '-'),
    wsTimestampDtDd: num('WS-TIMESTAMP-DT-DD', '9(02)'),
    filler3: alnum('FILLER', 'X(01)', ' '),
    wsTimestampTmHh: num('WS-TIMESTAMP-TM-HH', '9(02)'),
    filler4: alnum('FILLER', 'X(01)', ':'),
    wsTimestampTmMm: num('WS-TIMESTAMP-TM-MM', '9(02)'),
    filler5: alnum('FILLER', 'X(01)', ':'),
    wsTimestampTmSs: num('WS-TIMESTAMP-TM-SS', '9(02)'),
    filler6: alnum('FILLER', 'X(01)', '.'),
    wsTimestampTmMs6: num('WS-TIMESTAMP-TM-MS6', '9(06)'),
  }),
});
export type WsDateTime = RecordOf<typeof WsDateTimeLayout.root.fields>;

/** CSMSG01Y CCDA-COMMON-MESSAGES. */
export const CcdaCommonMessagesLayout = defineRecord('CCDA-COMMON-MESSAGES', {
  ccdaMsgThankYou: alnum('CCDA-MSG-THANK-YOU', 'X(50)', 'Thank you for using CardDemo application...      '),
  ccdaMsgInvalidKey: alnum('CCDA-MSG-INVALID-KEY', 'X(50)', 'Invalid key pressed. Please see below...         '),
});
export const CCDA_MSG_THANK_YOU = 'Thank you for using CardDemo application...      ';
export const CCDA_MSG_INVALID_KEY = 'Invalid key pressed. Please see below...         ';

/** CSMSG02Y ABEND-DATA. */
export const AbendDataLayout = defineRecord('ABEND-DATA', {
  abendCode: alnum('ABEND-CODE', 'X(4)'),
  abendCulprit: alnum('ABEND-CULPRIT', 'X(8)'),
  abendReason: alnum('ABEND-REASON', 'X(50)'),
  abendMsg: alnum('ABEND-MSG', 'X(72)'),
});
export type AbendData = RecordOf<typeof AbendDataLayout.root.fields>;

/** CODATECN CODATECN-REC: date conversion request/response for the COBDATFT assembler routine. */
export const CodatecnRecLayout = defineRecord('CODATECN-REC', {
  codatecnInRec: group('CODATECN-IN-REC', {
    codatecnType: alnum('CODATECN-TYPE', 'X'),
    codatecnInpDate: alnum('CODATECN-INP-DATE', 'X(20)'),
    codatecn1Inp: redefines(
      'codatecnInpDate',
      group('CODATECN-1INP', {
        codatecn1Yyyy: alnum('CODATECN-1YYYY', 'XXXX'),
        codatecn1Mm: alnum('CODATECN-1MM', 'XX'),
        codatecn1Dd: alnum('CODATECN-1DD', 'XX'),
        codatecn1Fil: alnum('CODATECN-1FIL', 'X(12)'),
      }),
    ),
    codatecn2Inp: redefines(
      'codatecnInpDate',
      group('CODATECN-2INP', {
        codatecn1OYyyy: alnum('CODATECN-1O-YYYY', 'XXXX'),
        codatecn1IS1: alnum('CODATECN-1I-S1', 'X'),
        codatecn1Mm: alnum('CODATECN-1MM', 'XX'),
        codatecn1IS2: alnum('CODATECN-1I-S2', 'X'),
        codatecn2Yy: alnum('CODATECN-2YY', 'XX'),
        codatecn2Fil: alnum('CODATECN-2FIL', 'X(10)'),
      }),
    ),
  }),
  codatecnOutRec: group('CODATECN-OUT-REC', {
    codatecnOuttype: alnum('CODATECN-OUTTYPE', 'X'),
    codatecn0utDate: alnum('CODATECN-0UT-DATE', 'X(20)'),
    codatecn1Out: redefines(
      'codatecn0utDate',
      group('CODATECN-1OUT', {
        codatecn1OYyyy: alnum('CODATECN-1O-YYYY', 'XXXX'),
        codatecn1OS1: alnum('CODATECN-1O-S1', 'X'),
        codatecn1OMm: alnum('CODATECN-1O-MM', 'XX'),
        codatecn1OS2: alnum('CODATECN-1O-S2', 'X'),
        codatecn1ODd: alnum('CODATECN-1O-DD', 'XX'),
        codatecn1OFil: alnum('CODATECN-1OFIl', 'X(10)'),
      }),
    ),
    codatecn2Out: redefines(
      'codatecn0utDate',
      group('CODATECN-2OUT', {
        codatecn2OYyyy: alnum('CODATECN-2O-YYYY', 'XXXX'),
        codatecn2OMm: alnum('CODATECN-2O-MM', 'XX'),
        codatecn2ODd: alnum('CODATECN-2O-DD', 'XX'),
        codatecn2OFil: alnum('CODATECN-2OFIl', 'X(12)'),
      }),
    ),
  }),
  codatecnErrorMsg: alnum('CODATECN-ERROR-MSG', 'X(38)'),
});
export type CodatecnRec = RecordOf<typeof CodatecnRecLayout.root.fields>;
export const CODATECN_YYYYMMDD_IN = '1';
export const CODATECN_YYYY_MM_DD_IN = '2';
export const CODATECN_YYYY_MM_DD_OP = '1';
export const CODATECN_YYYYMMDD_OP = '2';

/** CVCRD01Y CC-WORK-AREAS: card/account screen work areas. */
export const CcWorkAreasLayout = defineRecord('CC-WORK-AREAS', {
  ccWorkArea: group('CC-WORK-AREA', {
    ccardAid: alnum('CCARD-AID', 'X(5)'),
    ccardNextProg: alnum('CCARD-NEXT-PROG', 'X(8)'),
    ccardNextMapset: alnum('CCARD-NEXT-MAPSET', 'X(7)'),
    ccardNextMap: alnum('CCARD-NEXT-MAP', 'X(7)'),
    ccardErrorMsg: alnum('CCARD-ERROR-MSG', 'X(75)'),
    ccardReturnMsg: alnum('CCARD-RETURN-MSG', 'X(75)'),
    ccAcctId: alnum('CC-ACCT-ID', 'X(11)'),
    ccAcctIdN: redefines('ccAcctId', num('CC-ACCT-ID-N', '9(11)')),
    ccCardNum: alnum('CC-CARD-NUM', 'X(16)'),
    ccCardNumN: redefines('ccCardNum', num('CC-CARD-NUM-N', '9(16)')),
    ccCustId: alnum('CC-CUST-ID', 'X(09)'),
    ccCustIdN: redefines('ccCustId', num('CC-CUST-ID-N', '9(9)')),
  }),
});
export type CcWorkAreas = RecordOf<typeof CcWorkAreasLayout.root.fields>;
/** 88-levels on CCARD-AID. */
export const CCARD_AID = {
  ENTER: 'ENTER',
  CLEAR: 'CLEAR',
  PA1: 'PA1  ',
  PA2: 'PA2  ',
  PFK01: 'PFK01',
  PFK02: 'PFK02',
  PFK03: 'PFK03',
  PFK04: 'PFK04',
  PFK05: 'PFK05',
  PFK06: 'PFK06',
  PFK07: 'PFK07',
  PFK08: 'PFK08',
  PFK09: 'PFK09',
  PFK10: 'PFK10',
  PFK11: 'PFK11',
  PFK12: 'PFK12',
} as const;

/** COTTL01Y CCDA-SCREEN-TITLE. */
export const CcdaScreenTitleLayout = defineRecord('CCDA-SCREEN-TITLE', {
  ccdaTitle01: alnum('CCDA-TITLE01', 'X(40)', '      AWS Mainframe Modernization       '),
  ccdaTitle02: alnum('CCDA-TITLE02', 'X(40)', '              CardDemo                  '),
  ccdaThankYou: alnum('CCDA-THANK-YOU', 'X(40)', 'Thank you for using CCDA application... '),
});

/** One menu option (COADM02Y / COMEN02Y). */
export interface MenuOption {
  num: number;
  name: string;
  pgmName: string;
  /** COMEN02Y only: 'U' (user) / 'A' (admin). */
  usrType?: string;
}

/** COADM02Y CARDDEMO-ADMIN-MENU-OPTIONS (count 6, table OCCURS 9). */
export const ADMIN_MENU_OPTIONS: readonly MenuOption[] = Object.freeze([
  { num: 1, name: 'User List (Security)               ', pgmName: 'COUSR00C' },
  { num: 2, name: 'User Add (Security)                ', pgmName: 'COUSR01C' },
  { num: 3, name: 'User Update (Security)             ', pgmName: 'COUSR02C' },
  { num: 4, name: 'User Delete (Security)             ', pgmName: 'COUSR03C' },
  { num: 5, name: 'Transaction Type List/Update (Db2) ', pgmName: 'COTRTLIC' },
  { num: 6, name: 'Transaction Type Maintenance (Db2) ', pgmName: 'COTRTUPC' },
]);
export const ADMIN_MENU_OPT_COUNT = 6;

/**
 * COADM02Y storage layout. The OCCURS 9 table (9 x 45 = 405 bytes) REDEFINES the
 * 6 initialised entries (270 bytes), so the item spans 405 bytes; the 135 bytes
 * past the VALUE data have no VALUE clause and are initialised to spaces here.
 */
export const AdminMenuOptionsLayout = defineRecord('CARDDEMO-ADMIN-MENU-OPTIONS', {
  cdemoAdminOptCount: num('CDEMO-ADMIN-OPT-COUNT', '9(02)', { value: 6 }),
  cdemoAdminOptionsData: alnum(
    'CDEMO-ADMIN-OPTIONS-DATA',
    'X(405)',
    ADMIN_MENU_OPTIONS.map((o) => String(o.num).padStart(2, '0') + o.name + o.pgmName).join(''),
  ),
  cdemoAdminOptions: redefines(
    'cdemoAdminOptionsData',
    group('CDEMO-ADMIN-OPTIONS', {
      cdemoAdminOpt: occurs(
        9,
        group('CDEMO-ADMIN-OPT', {
          cdemoAdminOptNum: num('CDEMO-ADMIN-OPT-NUM', '9(02)'),
          cdemoAdminOptName: alnum('CDEMO-ADMIN-OPT-NAME', 'X(35)'),
          cdemoAdminOptPgmname: alnum('CDEMO-ADMIN-OPT-PGMNAME', 'X(08)'),
        }),
      ),
    }),
  ),
});

/** COMEN02Y CARDDEMO-MAIN-MENU-OPTIONS (count 11, table OCCURS 12). */
export const MAIN_MENU_OPTIONS: readonly MenuOption[] = Object.freeze([
  { num: 1, name: 'Account View                       ', pgmName: 'COACTVWC', usrType: 'U' },
  { num: 2, name: 'Account Update                     ', pgmName: 'COACTUPC', usrType: 'U' },
  { num: 3, name: 'Credit Card List                   ', pgmName: 'COCRDLIC', usrType: 'U' },
  { num: 4, name: 'Credit Card View                   ', pgmName: 'COCRDSLC', usrType: 'U' },
  { num: 5, name: 'Credit Card Update                 ', pgmName: 'COCRDUPC', usrType: 'U' },
  { num: 6, name: 'Transaction List                   ', pgmName: 'COTRN00C', usrType: 'U' },
  { num: 7, name: 'Transaction View                   ', pgmName: 'COTRN01C', usrType: 'U' },
  { num: 8, name: 'Transaction Add                    ', pgmName: 'COTRN02C', usrType: 'U' },
  { num: 9, name: 'Transaction Reports                ', pgmName: 'CORPT00C', usrType: 'U' },
  { num: 10, name: 'Bill Payment                       ', pgmName: 'COBIL00C', usrType: 'U' },
  { num: 11, name: 'Pending Authorization View         ', pgmName: 'COPAUS0C', usrType: 'U' },
]);
export const MAIN_MENU_OPT_COUNT = 11;

/**
 * COMEN02Y storage layout. The OCCURS 12 table (12 x 46 = 552 bytes) REDEFINES the
 * 11 initialised entries (506 bytes); the 46 bytes past the VALUE data are spaces here.
 */
export const MainMenuOptionsLayout = defineRecord('CARDDEMO-MAIN-MENU-OPTIONS', {
  cdemoMenuOptCount: num('CDEMO-MENU-OPT-COUNT', '9(02)', { value: 11 }),
  cdemoMenuOptionsData: alnum(
    'CDEMO-MENU-OPTIONS-DATA',
    'X(552)',
    MAIN_MENU_OPTIONS.map((o) => String(o.num).padStart(2, '0') + o.name + o.pgmName + (o.usrType ?? ' ')).join(''),
  ),
  cdemoMenuOptions: redefines(
    'cdemoMenuOptionsData',
    group('CDEMO-MENU-OPTIONS', {
      cdemoMenuOpt: occurs(
        12,
        group('CDEMO-MENU-OPT', {
          cdemoMenuOptNum: num('CDEMO-MENU-OPT-NUM', '9(02)'),
          cdemoMenuOptName: alnum('CDEMO-MENU-OPT-NAME', 'X(35)'),
          cdemoMenuOptPgmname: alnum('CDEMO-MENU-OPT-PGMNAME', 'X(08)'),
          cdemoMenuOptUsrtype: alnum('CDEMO-MENU-OPT-USRTYPE', 'X(01)'),
        }),
      ),
    }),
  ),
});

/** UNUSED1Y UNUSED-DATA (same shape as SEC-USER-DATA). */
export const UnusedDataLayout = defineRecord('UNUSED-DATA', {
  unusedId: alnum('UNUSED-ID', 'X(08)'),
  unusedFname: alnum('UNUSED-FNAME', 'X(20)'),
  unusedLname: alnum('UNUSED-LNAME', 'X(20)'),
  unusedPwd: alnum('UNUSED-PWD', 'X(08)'),
  unusedType: alnum('UNUSED-TYPE', 'X(01)'),
  unusedFiller: alnum('UNUSED-FILLER', 'X(23)'),
});

/** CSUTLDWY date-edit working storage (level-10 items, used with CSUTLDPY). */
export const CsutldwyLayout = defineRecord('CSUTLDWY', {
  wsEditDateCcyymmdd: group('WS-EDIT-DATE-CCYYMMDD', {
    wsEditDateCcyy: group('WS-EDIT-DATE-CCYY', {
      wsEditDateCc: alnum('WS-EDIT-DATE-CC', 'X(2)'),
      wsEditDateCcN: redefines('wsEditDateCc', num('WS-EDIT-DATE-CC-N', '9(2)')),
      wsEditDateYy: alnum('WS-EDIT-DATE-YY', 'X(2)'),
      wsEditDateYyN: redefines('wsEditDateYy', num('WS-EDIT-DATE-YY-N', '9(2)')),
    }),
    wsEditDateCcyyN: redefines('wsEditDateCcyy', num('WS-EDIT-DATE-CCYY-N', '9(4)')),
    wsEditDateMm: alnum('WS-EDIT-DATE-MM', 'X(2)'),
    wsEditDateMmN: redefines('wsEditDateMm', num('WS-EDIT-DATE-MM-N', '9(2)')),
    wsEditDateDd: alnum('WS-EDIT-DATE-DD', 'X(2)'),
    wsEditDateDdN: redefines('wsEditDateDd', num('WS-EDIT-DATE-DD-N', '9(2)')),
  }),
  wsEditDateCcyymmddN: redefines('wsEditDateCcyymmdd', num('WS-EDIT-DATE-CCYYMMDD-N', '9(8)')),
  wsEditDateBinary: num('WS-EDIT-DATE-BINARY', 'S9(9)', { usage: 'BINARY' }),
  wsCurrentDate: group('WS-CURRENT-DATE', {
    wsCurrentDateYyyymmdd: alnum('WS-CURRENT-DATE-YYYYMMDD', 'X(8)'),
    wsCurrentDateYyyymmddN: redefines('wsCurrentDateYyyymmdd', num('WS-CURRENT-DATE-YYYYMMDD-N', '9(8)')),
    wsCurrentDateBinary: num('WS-CURRENT-DATE-BINARY', 'S9(9)', { usage: 'BINARY' }),
  }),
  wsEditDateFlgs: group('WS-EDIT-DATE-FLGS', {
    wsEditYearFlg: alnum('WS-EDIT-YEAR-FLG', 'X(01)'),
    wsEditMonth: alnum('WS-EDIT-MONTH', 'X(01)'),
    wsEditDay: alnum('WS-EDIT-DAY', 'X(01)'),
  }),
  wsDateFormat: alnum('WS-DATE-FORMAT', 'X(08)', 'YYYYMMDD'),
  wsDateValidationResult: group('WS-DATE-VALIDATION-RESULT', {
    wsSeverity: alnum('WS-SEVERITY', 'X(04)'),
    wsSeverityN: redefines('wsSeverity', num('WS-SEVERITY-N', '9(4)')),
    filler: alnum('FILLER', 'X(11)', 'Mesg Code:'),
    wsMsgNo: alnum('WS-MSG-NO', 'X(04)'),
    wsMsgNoN: redefines('wsMsgNo', num('WS-MSG-NO-N', '9(4)')),
    filler2: alnum('FILLER', 'X(01)'),
    wsResult: alnum('WS-RESULT', 'X(15)'),
    filler3: alnum('FILLER', 'X(01)'),
    filler4: alnum('FILLER', 'X(09)', 'TstDate:'),
    wsDate: alnum('WS-DATE', 'X(10)'),
    filler5: alnum('FILLER', 'X(01)'),
    filler6: alnum('FILLER', 'X(10)', 'Mask used:'),
    wsDateFmt: alnum('WS-DATE-FMT', 'X(10)'),
    filler7: alnum('FILLER', 'X(01)'),
    filler8: alnum('FILLER', 'X(03)'),
  }),
});
