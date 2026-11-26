package com.carddemo.programs.codate01;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.carddemo.programs.codate01.copybook.MqConstants;
import com.carddemo.programs.codate01.copybook.MqGetMessageOptions;
import com.carddemo.programs.codate01.copybook.MqMessageDescriptor;
import com.carddemo.programs.codate01.copybook.MqObjectDescriptor;
import com.carddemo.programs.codate01.copybook.MqPutMessageOptions;
import com.carddemo.programs.codate01.copybook.MqTriggerMessage;
import java.time.LocalDateTime;
import org.junit.jupiter.api.Test;

class CopybookAndFieldTest {

    @Test
    void cobolMoveToAlphanumericPadsAndTruncates() {
        assertEquals("AB   ", CobolFields.alnum("AB", 5));
        assertEquals("ABCDE", CobolFields.alnum("ABCDEFG", 5));
        assertEquals("     ", CobolFields.alnum(null, 5));
    }

    @Test
    void moveSignedBinaryToUnsignedDisplayDropsSignAndHighOrderDigits() {
        assertEquals("02", CobolFields.unsignedDisplay(2, 2));
        assertEquals("02033", CobolFields.unsignedDisplay(2033, 5));
        assertEquals("23456", CobolFields.unsignedDisplay(123456, 5));
        assertEquals("00000013", CobolFields.unsignedDisplay(-13, 8));
        assertEquals("34567890", CobolFields.unsignedDisplay(1234567890, 8));
    }

    @Test
    void stringDelimitedBySizeStopsAtTargetLengthAndKeepsUnwrittenBytes() {
        assertEquals("ABCxx", CobolFields.stringInto("xxxxx", "AB", "C"));
        assertEquals("ABCDE", CobolFields.stringInto("xxxxx", "ABC", "DEFG"));
    }

    @Test
    void errDisplayIs113BytesWithTwoSpaceFillers() {
        MqErrDisplay d = new MqErrDisplay();
        d.errorPara("CICS RETRIEVE");
        d.applReturnMessage("A MESSAGE THAT IS FAR TOO LONG FOR 25");
        d.applConditionCode(2);
        d.applReasonCode(2085);
        d.applQueueName("CARD.DEMO.ERROR");
        String r = d.render();
        assertEquals(MqErrDisplay.LENGTH, r.length());
        assertEquals("CICS RETRIEVE              A MESSAGE THAT IS FAR TOO  02  02085  CARD.DEMO.ERROR",
                r.stripTrailing());
        d.initialize();
        assertEquals(" ".repeat(54) + "00  00000" + " ".repeat(50), d.render());
    }

    @Test
    void retrieveRespMessageFillsMqApplReturnMessageExactly() {
        MqErrDisplay d = new MqErrDisplay();
        d.stringIntoApplReturnMessage("RESP: ", "00000013", "00000000", "END");
        assertEquals("RESP: 0000001300000000END", d.applReturnMessage());
    }

    @Test
    void absTimeIsMillisecondsSince1900AndFormatTimeUsesSeparators() {
        assertEquals(0L, CicsTime.absTime(CicsTime.EPOCH));
        assertEquals(86_400_000L, CicsTime.absTime(LocalDateTime.of(1900, 1, 2, 0, 0)));
        long abs = CicsTime.absTime(LocalDateTime.of(2026, 11, 26, 17, 22, 5, 999_000_000));
        assertEquals("11-26-2026", CicsTime.mmddyyyy(abs, '-'));
        assertEquals("17:22:05", CicsTime.time(abs, ':'), "seconds are truncated, not rounded");
        assertEquals("11/26/2026", CicsTime.mmddyyyy(abs, '/'));
    }

    @Test
    void replyMessageLayoutHasNoSeparatorBetweenDateAndTimeLabel() {
        DateReplyMessage m = DateReplyMessage.of("11-26-2026", "17:22:05");
        assertEquals(1000, m.text().length());
        assertEquals("SYSTEM DATE : 11-26-2026SYSTEM TIME : 17:22:05", m.text().substring(0, 46));
        assertEquals(" ".repeat(954), m.text().substring(46));
        assertEquals("11-26-2026", m.systemDate());
        assertEquals("17:22:05", m.systemTime());
    }

    @Test
    void requestMsgCopyGroupMoveAndInitializeReplacingNumeric() {
        RequestMsgCopy init = RequestMsgCopy.initial();
        assertEquals(" ".repeat(4) + "00000000000" + " ".repeat(985), init.render());
        RequestMsgCopy c = RequestMsgCopy.fromMessage("INQA00000000042REST");
        assertEquals("INQA", c.func());
        assertEquals("00000000042", c.key());
        assertTrue(c.filler().startsWith("REST"));
        assertEquals(1000, c.render().length());
        RequestMsgCopy z = c.initializeNumericToZeroes();
        assertEquals("INQA", z.func(), "alphanumeric items are untouched by REPLACING NUMERIC");
        assertEquals("00000000000", z.key());
        assertFalse(RequestMsgCopy.fromMessage("INQAABC").keyIsNumeric());
    }

    @Test
    void cmqmdvInitialValues() {
        MqMessageDescriptor md = new MqMessageDescriptor();
        assertEquals("MD  ", md.strucId);
        assertEquals(1, md.version);
        assertEquals(MqConstants.MQMT_DATAGRAM, md.msgType);
        assertEquals(-1, md.expiry);
        assertEquals(785, md.encoding);
        assertEquals(0, md.codedCharSetId);
        assertEquals("        ", md.format());
        assertEquals(-1, md.priority);
        assertEquals(2, md.persistence);
        assertEquals(MqConstants.MQMI_NONE, md.msgId());
        assertEquals(MqConstants.MQCI_NONE, md.correlId());
        assertEquals(48, md.replyToQ().length());
        assertEquals(MqConstants.MQACT_NONE, md.accountingToken());
        assertEquals(1, md.msgSeqNumber);
        assertEquals(-1, md.originalLength);
        md.format("MQSTR");
        assertEquals("MQSTR   ", md.format());
        md.msgId("X".repeat(30));
        assertEquals(24, md.msgId().length());
        MqMessageDescriptor copy = md.copy();
        md.msgId("Y");
        assertEquals("X".repeat(24), copy.msgId());
    }

    @Test
    void cmqgmovCmqpmovCmqodvInitialValues() {
        MqGetMessageOptions gmo = new MqGetMessageOptions();
        assertEquals("GMO ", gmo.strucId);
        assertEquals(MqConstants.MQGMO_NO_WAIT, gmo.options);
        assertEquals(3, gmo.matchOptions);
        assertEquals(-1, gmo.returnedLength);
        assertEquals(MqConstants.MQMTOK_NONE, gmo.msgToken());

        MqPutMessageOptions pmo = new MqPutMessageOptions();
        assertEquals("PMO ", pmo.strucId);
        assertEquals(0, pmo.options);
        assertEquals(-1, pmo.timeout);
        assertEquals(9, pmo.pubLevel);

        MqObjectDescriptor od = new MqObjectDescriptor();
        assertEquals("OD  ", od.strucId);
        assertEquals(MqConstants.MQOT_Q, od.objectType);
        assertEquals("CSQ.*", od.dynamicQName().trim());
        assertEquals(MqConstants.MQSID_NONE, od.alternateSecurityId());
        od.objectName("CARD.DEMO.ERROR");
        assertEquals(48, od.objectName().length());
    }

    @Test
    void cmqtmlFieldsArePaddedToCopybookLengths() {
        MqTriggerMessage tm = MqTriggerMessage.forQueue("CARD.DEMO.REQUEST.DATE", "CDRD");
        assertEquals("TM  ", tm.strucId());
        assertEquals(48, tm.qName().length());
        assertEquals(64, tm.triggerData().length());
        assertEquals(256, tm.applId().length());
        assertEquals(MqTriggerMessage.MQAT_CICS, tm.applType());
        int total = tm.strucId().length() + 4 + tm.qName().length() + tm.processName().length()
                + tm.triggerData().length() + 4 + tm.applId().length() + tm.envData().length()
                + tm.userData().length();
        assertEquals(MqTriggerMessage.LENGTH, total);
    }

    @Test
    void mqConstantsUsedByCodate01() {
        assertEquals(0, MqConstants.MQCC_OK);
        assertEquals(2033, MqConstants.MQRC_NO_MSG_AVAILABLE);
        assertEquals(8194 + 128, MqConstants.MQOO_INPUT_SHARED + MqConstants.MQOO_SAVE_ALL_CONTEXT
                + MqConstants.MQOO_FAIL_IF_QUIESCING);
        assertEquals("MQSTR   ", MqConstants.MQFMT_STRING);
        assertEquals(24, MqConstants.MQMI_NONE.length());
    }
}
