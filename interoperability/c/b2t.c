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
#include <higherc/alloc.h>

#include "erl_interface.h"

#include "eterm.h"

static int doit(const char *buf, int len, hcns(bool) eof)
{
	struct eterm *h;
	int index = 0;
	int version;

	assert(eof == 1);  /* eof is expected since the buffer is
			    * sufficiently large and in erlang's
			    * distribution protocol we now the buffer
			    * size in advance */


	assert(ei_decode_version(buf, &index, &version) == 0);
	assert(version == 131 /* 130 in erlang 4.2 */);

	h = eterm_decode(buf, len, &index);

	fflush(stderr);

	eterm_show(0, h);

	/* clean up
	 */

	eterm_free(h);

	fprintf(stderr, "processed %i bytes\n", len);
	return len;
}

int main(int argc, char **argv)
{
	int bufsz = 1024 * 1024;   /* enough size to read any practical term */
	void *buf;
	int fd;

	HC_ALLOC(buf, bufsz);

	fprintf(stderr, "reading %s\n", "t2b.escript.0.bin");
	assert((fd = open("t2b.escript.0.bin", O_RDONLY)) >= 0);
	fprintf(stderr, "total bytes read: %i\n", hcns(readfd)(fd, buf, bufsz, doit));
	close(fd);

	fprintf(stderr, "reading %s\n", "t2b.escript.1.bin");
	assert((fd = open("t2b.escript.1.bin", O_RDONLY)) >= 0);
	fprintf(stderr, "total bytes read: %i\n", hcns(readfd)(fd, buf, bufsz, doit));
	close(fd);

	fprintf(stderr, "reading %s\n", "t2b.c.bin");
	assert((fd = open("t2b.c.bin", O_RDONLY)) >= 0);
	fprintf(stderr, "total bytes read: %i\n", hcns(readfd)(fd, buf, bufsz, doit));
	close(fd);

	fprintf(stderr, "reading %s\n", "108+107.bin");
	assert((fd = open("108+107.bin", O_RDONLY)) >= 0);
	fprintf(stderr, "total bytes read: %i\n", hcns(readfd)(fd, buf, bufsz, doit));
	close(fd);

	fprintf(stderr, "reading %s\n", "108+108+107.bin");
	assert((fd = open("108+108+107.bin", O_RDONLY)) >= 0);
	fprintf(stderr, "total bytes read: %i\n", hcns(readfd)(fd, buf, bufsz, doit));
	close(fd);

	HC_FREE(buf);

	return 0;
}
