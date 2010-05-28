#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>  /* /usr/include/asm-generic/errno.h */
#include <assert.h>

#include <higherc/higherc.h>
#include <higherc/byte.h>
#include <higherc/bytewise.h>
#include <higherc/readfd.h>

#include "erl_interface.h"
#include "ei.h"

/* safe_read: when you must fail, fail noisily and as soon as possible
 */
static int safe_read(int fd, char *buf, int count)
{
	ssize_t r = read(fd, buf, count);
	if (r < 0) {
		assert(r == -1);  // just to detect weird systems/weird bugs
		fprintf(stderr, "read() = %li, errno: %i\n", (long int) r, errno);
		fflush(stderr);
		perror("read()");
		exit(1);
	}
	if (r == 0 && errno) {
		if (errno != ENOTTY /* Not a typewriter */) {
			/* interesting values to check: EWOULDBLOCK
			 * and EAGAIN for non-blocking descriptors */
			fprintf(stderr, "read() = 0 and errno set, errno: %i\n", errno);
			fflush(stderr);
			perror("read()");
			exit(1);
		}
	}
	return r;
}

static int safe_write(int fd, char *buf, int count)
{
	ssize_t r = write(fd, buf, count);
	if (r < 0) {
		assert(r == -1);  // just to detect weird systems/weird bugs
		fprintf(stderr, "write() = %li, errno: %i\n", (long int) r, errno);
		fflush(stderr);
		perror("write()");
		exit(1);
	}
	if (r == 0 && errno) {
		if (errno != ENOTTY /* Not a typewriter */) {
			/* interesting values to check: EWOULDBLOCK
			 * and EAGAIN for non-blocking descriptors */
			fprintf(stderr, "write() = 0 and errno set, errno: %i\n", errno);
			fflush(stderr);
			perror("write()");
			exit(1);
		}
	}
	return r;
}

static int read_exact(int fd, void *buf, int len)
{
	int i, got=0;

	do {
		if ((i = safe_read(fd, buf+got, len-got)) <= 0)
			return i;
		got += i;
	} while (got<len);

	return len;
}

static int write_exact(int fd, void *buf, int len)
{
	int i, wrote = 0;

	do {
		if ((i = safe_write(fd, buf+wrote, len-wrote)) <= 0)
			return i;
		wrote += i;
	} while (wrote<len);

	return len;
}

int main(int argc, char **argv)
{
	int plen; /* packet length */
	int n, eof;
	char buf0[4];
	char *buf;
	int buflen = 1024 * 1024;

	HC_ALLOC(buf, buflen);

	for (;;) {
		n = safe_read(0, buf0, sizeof(buf0));
		eof = n == 0;
		if (eof) {
			fprintf(stderr, "eof\n");
			break;
		}
		plen = HC_GET_BE4(buf0);
		fprintf(stderr, "packet size %i\n", plen);

		assert(plen <= buflen);

		n = read_exact(0, buf, plen);
		assert(plen == n);

		/* response
		 */

		HC_PUT_BE4(buf0, 2);
		assert(write_exact(1, buf0, 4) == 4);
		n = write_exact(1, "Ok", 2);
		assert(n == 2);
	}

	HC_FREE(buf);

	return 0;
}
