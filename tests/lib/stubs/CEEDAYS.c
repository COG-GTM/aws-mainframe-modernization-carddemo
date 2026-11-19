/*
 * Test double for the IBM Language Environment CEEDAYS callable service
 * (convert a date to a Lilian day number) so that CSUTLDTC can run under
 * GnuCOBOL.  It models the documented behaviour/feedback codes for the
 * picture strings CardDemo uses (YYYY, MM, DD tokens plus separators):
 *
 *   CEE000 (all zero) ..... date valid, Lilian day returned
 *   CEE2EB  msg 2507 ...... insufficient data (length mismatch / blank)
 *   CEE2EC  msg 2508 ...... day not valid for month/year
 *   CEE2EH  msg 2513 ...... outside Lilian range (1582-10-15..9999-12-31)
 *   CEE2EL  msg 2517 ...... month not 01..12
 *   CEE2EM  msg 2518 ...... unsupported picture string
 *   CEE2EO  msg 2520 ...... non-numeric data in a date token
 *
 * Arguments follow the LE calling convention: VSTRINGs (2-byte big-endian
 * length + text), fullword big-endian binary Lilian output, and a 12-byte
 * feedback token (sev, msgno, case/sev ctl, facility "CEE" in EBCDIC,
 * ISI).  Trailing blanks of the date and picture are ignored.
 */
#include <string.h>

static void put16(unsigned char *p, int v) { p[0] = (v >> 8) & 0xff; p[1] = v & 0xff; }
static void put32(unsigned char *p, long v)
{
    p[0] = (v >> 24) & 0xff; p[1] = (v >> 16) & 0xff;
    p[2] = (v >> 8) & 0xff;  p[3] = v & 0xff;
}

static void feedback(unsigned char *fc, int msgno)
{
    memset(fc, 0, 12);
    if (msgno == 0)
        return;
    put16(fc, 3);            /* severity 3 (error)            */
    put16(fc + 2, msgno);    /* message number                */
    fc[4] = 0x59;            /* case 1, severity 3, control 1 */
    fc[5] = 0xC3; fc[6] = 0xC5; fc[7] = 0xC5;   /* 'CEE' EBCDIC  */
}

static long days_from_civil(long y, long m, long d)
{
    y -= m <= 2;
    long era = (y >= 0 ? y : y - 399) / 400;
    long yoe = y - era * 400;
    long doy = (153 * (m + (m > 2 ? -3 : 9)) + 2) / 5 + d - 1;
    long doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    return era * 146097 + doe;
}

static int is_leap(long y) { return (y % 4 == 0 && y % 100 != 0) || y % 400 == 0; }

static int vstr(const unsigned char *v, char *out)
{
    int len = (v[0] << 8) | v[1];
    if (len < 0 || len > 256) len = 0;
    memcpy(out, v + 2, len);
    while (len > 0 && out[len - 1] == ' ') len--;
    out[len] = 0;
    return len;
}

int CEEDAYS(unsigned char *date_v, unsigned char *pic_v,
            unsigned char *lilian, unsigned char *fc)
{
    char date[257], pic[257];
    int dlen = vstr(date_v, date);
    int plen = vstr(pic_v, pic);
    int ypos = -1, mpos = -1, dpos = -1, i;

    put32(lilian, 0);
    for (i = 0; i < plen;) {
        if (!strncmp(pic + i, "YYYY", 4) && ypos < 0) { ypos = i; i += 4; }
        else if (!strncmp(pic + i, "MM", 2) && mpos < 0) { mpos = i; i += 2; }
        else if (!strncmp(pic + i, "DD", 2) && dpos < 0) { dpos = i; i += 2; }
        else if (strchr("-/. ", pic[i])) i++;
        else { feedback(fc, 2518); return 0; }
    }
    if (ypos < 0 || mpos < 0 || dpos < 0) { feedback(fc, 2518); return 0; }
    if (dlen == 0 || dlen != plen) { feedback(fc, 2507); return 0; }

    for (i = 0; i < 4; i++) if (date[ypos + i] < '0' || date[ypos + i] > '9') { feedback(fc, 2520); return 0; }
    for (i = 0; i < 2; i++) {
        if (date[mpos + i] < '0' || date[mpos + i] > '9') { feedback(fc, 2520); return 0; }
        if (date[dpos + i] < '0' || date[dpos + i] > '9') { feedback(fc, 2520); return 0; }
    }
    long y = (date[ypos] - '0') * 1000 + (date[ypos + 1] - '0') * 100 +
             (date[ypos + 2] - '0') * 10 + (date[ypos + 3] - '0');
    long m = (date[mpos] - '0') * 10 + (date[mpos + 1] - '0');
    long d = (date[dpos] - '0') * 10 + (date[dpos + 1] - '0');
    static const int mdays[] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

    if (m < 1 || m > 12) { feedback(fc, 2517); return 0; }
    if (d < 1 || d > mdays[m - 1] + (m == 2 && is_leap(y))) { feedback(fc, 2508); return 0; }
    if (y < 1582 || (y == 1582 && (m < 10 || (m == 10 && d < 15)))) { feedback(fc, 2513); return 0; }

    put32(lilian, days_from_civil(y, m, d) - days_from_civil(1582, 10, 14));
    feedback(fc, 0);
    return 0;
}
