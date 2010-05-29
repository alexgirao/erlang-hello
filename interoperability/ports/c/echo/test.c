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
#include <higherc/readfd.h>

#include "erl_interface.h"
#include "ei.h"

// otp_src_R13B04/lib/erl_interface/src/misc/ei_decode_term.c

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
	int i, got = 0;

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

//int ei_get_type(const char *buf, const int *index, int *type, int *size);

static void buf2file(char *fname, void *buf, int buflen)
{
	int fd = open(fname, O_CREAT | O_WRONLY | O_TRUNC);
	assert(fd >= 0);

	assert(write_exact(fd, buf, buflen) == buflen);

	close(fd);
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
		/* read packet len
		 */

		n = safe_read(0, buf0, sizeof(buf0));
		eof = n == 0;
		if (eof) {
			fprintf(stderr, "eof\n");
			break;
		}
		plen = HC_GET_BE4(buf0);
		fprintf(stderr, "packet size %i\n", plen);

		assert(plen <= buflen);

		/* read packet data
		 */

		n = read_exact(0, buf, plen);
		assert(plen == n);

		//buf2file(__FILE__ ".packet", buf, plen);

		{
			int version;
			int index;
			ei_term e[1];
			char *str;
			int r;

			index = 0;

			r = ei_decode_version(buf, &index, &version);
			assert(r == 0);
			assert(version == 131);

			int noloop = 100;

			while (index < plen && noloop) {
				noloop--;

				r = ei_decode_ei_term(buf, &index, e);
				assert(r == 0 || r == 1);

				switch (e->ei_type) {
				case ERL_SMALL_INTEGER_EXT:
					break;
				case ERL_INTEGER_EXT:
					break;
				case ERL_FLOAT_EXT:
					break;
				case ERL_ATOM_EXT:
					break;
				case ERL_REFERENCE_EXT:
					break;
				case ERL_NEW_REFERENCE_EXT:
					break;
				case ERL_PORT_EXT:
					break;
				case ERL_PID_EXT:
					break;
				case ERL_SMALL_TUPLE_EXT:
					break;
				case ERL_LARGE_TUPLE_EXT:
					break;
				case ERL_NIL_EXT:
					break;
				case ERL_STRING_EXT:
					/* int ei_decode_string(const char *buf, int *index, char *p)
					 */
					HC_ALLOC(str, e->size + 1);
					assert(ei_decode_string(buf, &index, str) == 0);
					assert(str[0] = 1);
					assert(str[1] = 2);
					assert(str[2] = 3);
					HC_FREE(str);
				case ERL_LIST_EXT:
					break;
				case ERL_BINARY_EXT:
					break;
				case ERL_SMALL_BIG_EXT:
					break;
				case ERL_LARGE_BIG_EXT:
					break;
				case ERL_PASS_THROUGH:
					break;
				case ERL_NEW_CACHE:
					break;
				case ERL_CACHED_ATOM:
					break;
				default:
					fprintf(stderr, "unknown type: i=%i, t=%i (%c), a=%i, s=%i\n", index, e->ei_type, e->ei_type, e->arity, e->size);
					continue;
				}
				fprintf(stderr, "known type: i=%i, t=%i (%c), a=%i, s=%i\n", index, e->ei_type, e->ei_type, e->arity, e->size);
			}
		}

		/* response
		 */

		HC_PUT_BE4(buf0, 2);
		assert(write_exact(1, buf0, 4) == 4);
		n = write_exact(1, "Ok", 2);
		assert(n == 2);

		break;
	}

	HC_FREE(buf);

	return 0;
}
