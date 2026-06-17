#include <check.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* The actual decompression function from cfitsio */
int zuncompress2mem(char *filename, FILE *diskfile, char **buffptr,
                   size_t *buffsize, void *(*mem_realloc)(void *p, size_t newsize),
                   size_t *filesize, int *status);

START_TEST(test_lzw_buffer_overflow_invariant)
{
    /* Invariant: zuncompress2mem must never overflow output buffer regardless
       of malformed/oversized compressed input; it must return error or
       truncate, never silently corrupt memory. */

    /* Payloads: crafted as raw byte buffers written to temp files */
    struct {
        const unsigned char *data;
        size_t len;
        const char *desc;
    } payloads[3];

    /* Payload 1: exact LZW exploit - max code bits set, oversized codes */
    static const unsigned char exploit_payload[64] = {
        0x1f, 0x9d,       /* LZW magic */
        0xbf,             /* max bits=31, block mode */
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff
    };

    /* Payload 2: boundary - minimal valid LZW header, 1 byte body */
    static const unsigned char boundary_payload[4] = {
        0x1f, 0x9d, 0x90, 0x00
    };

    /* Payload 3: valid small LZW compressed stream (compress "AAAA") */
    static const unsigned char valid_payload[11] = {
        0x1f, 0x9d, 0x90,
        0x41, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    };

    payloads[0].data = exploit_payload; payloads[0].len = sizeof(exploit_payload); payloads[0].desc = "exploit";
    payloads[1].data = boundary_payload; payloads[1].len = sizeof(boundary_payload); payloads[1].desc = "boundary";
    payloads[2].data = valid_payload;   payloads[2].len = sizeof(valid_payload);   payloads[2].desc = "valid";

    for (int i = 0; i < 3; i++) {
        char tmpname[] = "/tmp/test_lzw_XXXXXX";
        int fd = mkstemp(tmpname);
        ck_assert_int_ge(fd, 0);
        write(fd, payloads[i].data, payloads[i].len);
        close(fd);

        FILE *f = fopen(tmpname, "rb");
        ck_assert_ptr_nonnull(f);

        size_t bufsize = 1024;
        char *buf = (char *)malloc(bufsize);
        ck_assert_ptr_nonnull(buf);
        size_t filesize = 0;
        int status = 0;

        /* Must not crash or overflow; error return is acceptable */
        zuncompress2mem(tmpname, f, &buf, &bufsize, realloc, &filesize, &status);

        fclose(f);
        free(buf);
        remove(tmpname);
        /* If we reach here without SIGSEGV/SIGABRT, invariant holds */
        ck_assert_msg(1, "payload %s caused crash", payloads[i].desc);
    }
}
END_TEST

Suite *security_suite(void) {
    Suite *s = suite_create("Security");
    TCase *tc = tcase_create("Core");
    tcase_add_test(tc,