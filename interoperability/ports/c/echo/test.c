#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>  /* /usr/include/asm-generic/errno.h */
#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <higherc/higherc.h>
#include <higherc/byte.h>
#include <higherc/bytewise.h>
#include <higherc/str.h>
#include <higherc/s.h>

#include "erl_interface.h"

#include "../../../c/eterm.h"

#define TEST_BIN "test.bin"

static void buf2file(char *fname, void *buf, int buflen)
{
	int fd = open(fname,
		O_CREAT | O_WRONLY | O_TRUNC,
		S_IRUSR | S_IWUSR
		);
	assert(fd >= 0);

	assert(hcns(write_exact)(fd, buf, buflen) == buflen);

	close(fd);
}

static void doit(const char *buf, int len)
{
	struct eterm *h;
	int index = 0;
	int version;

	assert(ei_decode_version(buf, &index, &version) == 0);
	assert(version == 131 /* 130 in erlang 4.2 */);

	h = eterm_decode(buf, len, &index);

	fflush(stderr);

	eterm_show(0, h);

	/* clean up
	 */

	eterm_free(h);

	fprintf(stderr, "processed %i bytes\n", index);
}

static int doit0(const char *buf, int len, hcns(bool) eof)
{
	buf2file(TEST_BIN, (void*)buf, len);
	return len;
}

static void doit1(const char *buf, int len)
{
	assert(len > 0);
}

/* calls ``doit'' for each received packet
 */
int hcns(readfd_packet_be4)(int fd, void *buf, int bufsz, void (*doit)(const char *buf, int len))
{
	hcns(u1) plen0[4]; /* packet len */
	hcns(u4) plen;
	int len;

	assert(bufsz > 0);
	assert(errno == 0 || errno == ENOTTY);

	assert(hcns(read_exact)(fd, plen0, sizeof(plen0)) == 4);
	plen = HC_GET_BE4(plen0);

	fprintf(stderr, "packet length: %i\n", plen);

	assert(bufsz >= plen);

	len = hcns(read_exact)(fd, buf, plen);
	if (len < plen) {
		HC_FATAL("couldn't read all packet, read %i, expected %i", len, plen);
	}
	assert(len == plen);

	doit(buf, len);

	return len + 4;
}

int main(int argc, char **argv)
{
	int bufsz = 1024 * 1024;   /* enough size to read any practical term */
	void *buf;
	int fd;

	HC_ALLOC(buf, bufsz);

#if 0
	fd = 0;
	fprintf(stderr, "total bytes read: %i\n", hcns(readfd)(fd, buf, bufsz, doit0));
#else
	fprintf(stderr, "reading " TEST_BIN "\n");
	assert((fd = open(TEST_BIN, O_RDONLY)) >= 0);
	fprintf(stderr, "total bytes read: %i\n", hcns(readfd_packet_be4)(fd, buf, bufsz, doit));
	close(fd);
#endif

	HC_FREE(buf);

	return 0;
}
