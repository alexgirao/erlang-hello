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

#include <higherc/higherc.h>
#include <higherc/byte.h>
#include <higherc/bytewise.h>
#include <higherc/str.h>
#include <higherc/s.h>

#include "erl_interface.h"

#include "../../../c/eterm.h"

/* #define TEST_BIN "test.bin" */

#ifdef TEST_BIN

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

static int doit(const char *buf, int len, hcns(bool) eof)
{
	buf2file(TEST_BIN, (void*)buf, len);
	return len;
}

#else

static void do_echo(HC_ST_S *out, struct eterm *h)
{
	struct eterm *t;
	struct eterm_iter i[1];

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
		case ERL_TINY_ATOM_EXT:
			encode_atomn(out, t->value.tinystr, t->len);
			break;
		case ERL_TINY_STRING_EXT:
			encode_stringn(out, t->value.tinystr, t->len);
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
			HC_FATAL("unimplemented type %i", t->type);
		}
	}
	eterm_end(i);
}

static void doit(const char *buf, int len)
{
	struct eterm *h;
	int index = 0;
	int version;
	HC_DEF_S(out);
	hcns(u1) plen[4];

	assert(ei_decode_version(buf, &index, &version) == 0);
	assert(version == 131 /* 130 in erlang 4.2 */);

	h = eterm_decode(buf, len, &index);

	encode_version(out);
	do_echo(out, h);

	HC_PUT_BE4(plen, out->len);

	fprintf(stderr, "spitting out %i bytes\n", out->len);
	fflush(stderr);

	assert(hcns(write_exact)(1, plen, 4) == 4);
	assert(hcns(write_exact)(1, out->s, out->len) == out->len);

	hcns(s_free)(out);

	/* clean up
	 */

	eterm_free(h);

	fprintf(stderr, "processed %i bytes\n", index);
}

#endif

int main(int argc, char **argv)
{
	int bufsz = 1024 * 1024;   /* enough size to read any practical term */
	void *buf;
	int fd;

	HC_ALLOC(buf, bufsz);

#ifdef TEST_BIN
	fd = 0;
	fprintf(stderr, "total bytes read: %i\n", hcns(readfd)(fd, buf, bufsz, doit));
#else
	fd = 0;
	for (;;) {
		int n;
		int plen = hcns(readfd_be4)(fd, buf);
		if (plen == -1) {
			/* EOF
			 */
			break;
		}
		fprintf(stderr, "packet length: %i\n", plen);

		assert(plen <= bufsz); /* ensure data fits buffer */

		n = hcns(read_exact)(fd, buf, plen);
		assert(n == plen);
		
		doit(buf, plen);
	}
#endif

	HC_FREE(buf);

	return 0;
}
