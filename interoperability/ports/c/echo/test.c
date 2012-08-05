/* ./test < test.bin >/dev/null
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>  /* /usr/include/asm-generic/errno.h */
#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "str.h"

#include "erl_interface.h"

#include "str.h"
#include "item.h"

#include "eterm.h"

/*
 */

//#define TEST_BIN "test.bin"

/*
 */

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

#ifndef TEST_BIN
static int read_exact(int fd, void *buf, int len)
{
	int i, got=0;
	do {
		if ((i = read(fd, buf + got, len - got)) <= 0) return i;
		got += i;
	} while (got < len);
	return len;
}
#endif

static int write_exact(int fd, void *buf, int len)
{
	int i, wrote = 0;
	do {
		if ((i = write(fd, buf + wrote, len - wrote)) <= 0) return i;
		wrote += i;
	} while (wrote < len);
	return len;
}

#ifdef TEST_BIN

static void buf2file(char *fname, void *buf, int buflen)
{
	int fd = open(fname,
		O_CREAT | O_WRONLY | O_TRUNC,
		S_IRUSR | S_IWUSR
		);
	assert(fd >= 0);

	assert(write_exact(fd, buf, buflen) == buflen);

	close(fd);
}

static int doit(const char *buf, int len, int eof)
{
	buf2file(TEST_BIN, (void*)buf, len);
	return len;
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

#else /* matches: #ifdef TEST_BIN */

static void do_echo(struct str *out, struct eterm *h)
{
	struct eterm *t;
	struct eterm_iterator i[1];

	eterm_forward(i, h);
	while ((t = eterm_next(i))) {
		switch (t->type) {
		case ERL_SMALL_INTEGER_EXT:
		case ERL_INTEGER_EXT:
			encode_long(out, t->value.i_val);
			break;
		case 70: /* newFloatTag */
		case ERL_FLOAT_EXT:
			encode_double_type70(out, t->value.d_val);
			break;
		case ERL_ATOM_EXT:
			encode_atomn(out, t->value.str->s, t->len);
			break;
		case ERL_STRING_EXT:
			encode_stringn(out, t->value.str->s, t->len);
			break;
		case ERL_BINARY_EXT:
			encode_binary(out, t->value.str->s, t->len);
			break;
		case ERL_TINY_ATOM_EXT:
			encode_atomn(out, t->value.tinystr, t->len);
			break;
		case ERL_TINY_STRING_EXT:
			encode_stringn(out, t->value.tinystr, t->len);
			break;
		case ERL_TINY_BINARY_EXT:
			encode_binary(out, t->value.tinystr, t->len);
			break;
		case ERL_SMALL_TUPLE_EXT:
		case ERL_LARGE_TUPLE_EXT:
			encode_tuple_header(out, t->len);
			do_echo(out, t->value.children);
			break;
		case ERL_LIST_EXT:
			encode_list_header(out, t->len);
			do_echo(out, t->value.children);
			break;
		case ERL_NIL_EXT:
			encode_empty_list(out);
			break;
		default:
			DIE("unimplemented type %i", t->type);
		}
	}
	eterm_end(i);
}

static void doit(const char *buf, int len)
{
	struct eterm *h;
	int index = 0;
	int version;
	DEFINE_STR(out);
	unsigned char plen[4];

	assert(ei_decode_version(buf, &index, &version) == 0);
	assert(version == 131 /* 130 in erlang 4.2 */);

	h = eterm_decode(buf, len, &index);

	encode_version(out);
	do_echo(out, h);

	*(uint32_t*)(plen) = htonl(out->len);

	fprintf(stderr, "spitting out %i bytes\n", out->len);
	fflush(stderr);

	assert(write_exact(1, plen, 4) == 4);
	assert(write_exact(1, out->s, out->len) == out->len);

	str_free(out);

	/* clean up
	 */

	eterm_free(h);

	fprintf(stderr, "processed %i bytes\n", index);
}

static uint32_t readfd_be4(int fd)
{
	uint32_t val;
	int len;
	len = read_exact(fd, &val, sizeof(val));
	return ntohl(val);
}

#endif /* matches: #else / * matches: #ifdef TEST_BIN */

int main(int argc, char **argv)
{
	int bufsz = 1024 * 1024;   /* enough size to read any practical term */
	void *buf;
	int fd;

	buf = malloc(bufsz);
	assert(buf);

#ifdef TEST_BIN
	fd = 0;
	fprintf(stderr, "total bytes read: %i\n", readfd(fd, buf, bufsz, doit));
#else
	fd = 0;
	for (;;) {
		int n;
		int plen = readfd_be4(fd);
		if (plen == -1) {
			/* EOF
			 */
			break;
		}
		fprintf(stderr, "packet length: %i\n", plen);

		assert(plen <= bufsz); /* ensure data fits buffer */

		n = read_exact(fd, buf, plen);
		assert(n == plen);
		
		doit(buf, plen);
	}
#endif

	free(buf);

	return 0;
}
