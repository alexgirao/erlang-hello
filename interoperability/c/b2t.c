#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>  /* /usr/include/asm-generic/errno.h */
#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "erl_interface.h"

#include "str.h"
#include "item.h"

#include "eterm.h"

#if defined (__STDC_VERSION__) && __STDC_VERSION__ >= 199901L
# define DIE(...)	    _p_fatal(__FILE__, __LINE__, __VA_ARGS__)
#elif defined (__GNUC__)
# define DIE(fmt...)   _p_fatal(__FILE__, __LINE__, fmt)
#endif

static void _p_fatal(char *file, int line, char *fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	fprintf(stderr, "fatal-error: %s: %i: ", file, (int)line);
	vfprintf(stderr, fmt, args);
	fprintf(stderr, "\n");
	fflush(stderr);
	va_end(args);
	exit(1);
}

static void b_copyl(void *to, int n, const void *from)
{
	char *t = to;
	const char *f = from;
	for (;;) {
		if (!n)
			return;
		*t++ = *f++;
		--n;

		if (!n)
			return;
		*t++ = *f++;
		--n;

		if (!n)
			return;
		*t++ = *f++;
		--n;

		if (!n)
			return;
		*t++ = *f++;
		--n;
	}
}

static void b_copyr(void *to, int n, const void *from)
{
	char *t = (char*)to + n;
	const char *f = (char*)from + n;
	for (;;) {
		if (!n)
			return;
		*--t = *--f;
		--n;

		if (!n)
			return;
		*--t = *--f;
		--n;

		if (!n)
			return;
		*--t = *--f;
		--n;

		if (!n)
			return;
		*--t = *--f;
		--n;
	}
}

static void b_copy(void *to, int n, const void *from)
{
	if (from > to) {
		return b_copyl(to, n, from);
	} else if (from < to) {
		return b_copyr(to, n, from);
	}
}

static int readfd(int fd, void *buf, int bufsz, int (*doit)(const char *buf, int len, int eof))
{
	int total = 0;
	int len = 0;

	assert(bufsz > 0);

	for (;;) {
		int n;
		int eof = 0;
		if (len < bufsz) {
			do {
				assert((n = read(fd, buf + len, bufsz - len)) >= 0);
				eof = n == 0;
				if (eof) break;
				len += n;
			} while (len < bufsz);
			assert(len <= bufsz);
			if (len == 0) {
				assert(eof);
				break;
			}
		}

		/* process data
		 */
		n = doit(buf, len, eof);

		assert(n >= 0); /* 0 means need more data */
		assert(n <= len);
		assert(!eof || n == len); /* processing function MUST
					   * consume ALL data upon
					   * EOF */

		if (n == 0 && len == bufsz) {
			DIE("not enough space in buffer");
		}

		if (eof) {
			total += len;
			break;
		} else if (n > 0) {
			/* data was consumed by processing function
			 */
			if (n < len) {
				/* copy unprocessed data to start of
				 * buffer (shift left)
				 */
				b_copy(buf, len - n, buf + n);
			}
			len -= n;
			total += n;
		}
    	}

	return total;
}

static int doit(const char *buf, int len, int eof)
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

	buf = malloc(bufsz);
	assert(buf);

	fprintf(stderr, "reading %s\n", "t2b.escript.0.bin");
	assert((fd = open("t2b.escript.0.bin", O_RDONLY)) >= 0);
	fprintf(stderr, "total bytes read: %i\n", readfd(fd, buf, bufsz, doit));
	close(fd);

	fprintf(stderr, "reading %s\n", "t2b.escript.1.bin");
	assert((fd = open("t2b.escript.1.bin", O_RDONLY)) >= 0);
	fprintf(stderr, "total bytes read: %i\n", readfd(fd, buf, bufsz, doit));
	close(fd);

	fprintf(stderr, "reading %s\n", "t2b.c.bin");
	assert((fd = open("t2b.c.bin", O_RDONLY)) >= 0);
	fprintf(stderr, "total bytes read: %i\n", readfd(fd, buf, bufsz, doit));
	close(fd);

	fprintf(stderr, "reading %s\n", "108+107.bin");
	assert((fd = open("108+107.bin", O_RDONLY)) >= 0);
	fprintf(stderr, "total bytes read: %i\n", readfd(fd, buf, bufsz, doit));
	close(fd);

	fprintf(stderr, "reading %s\n", "108+108+107.bin");
	assert((fd = open("108+108+107.bin", O_RDONLY)) >= 0);
	fprintf(stderr, "total bytes read: %i\n", readfd(fd, buf, bufsz, doit));
	close(fd);

	free(buf);
	buf = NULL;

	return 0;
}
