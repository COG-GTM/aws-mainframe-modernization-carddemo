/*
 * Stand-in for the LE service CEEDAYS, so app/cbl/CSUTLDTC.cbl runs under GnuCOBOL.
 * It returns the 12-byte feedback code given in CEEDAYS_FC (24 hex digits) and does not
 * judge the date: it exists only to capture how CSUTLDTC maps each feedback token to LS-RESULT.
 */
#include <stdlib.h>
#include <string.h>

static int hex(char c) {
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'A' && c <= 'F') return c - 'A' + 10;
    if (c >= 'a' && c <= 'f') return c - 'a' + 10;
    return 0;
}

int CEEDAYS(void *date, void *picture, unsigned char *lilian, unsigned char *fc) {
    const char *h = getenv("CEEDAYS_FC");
    (void) date;
    (void) picture;
    memset(lilian, 0, 4);
    memset(fc, 0, 12);
    if (h != NULL) {
        size_t n = strlen(h) / 2;
        for (size_t i = 0; i < n && i < 12; i++) {
            fc[i] = (unsigned char) (hex(h[2 * i]) << 4 | hex(h[2 * i + 1]));
        }
    }
    return 0;
}
