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

#include "common-inc.c"

/* processing function, return amount of successfully processed data
 */
static int doit(const char *buf, int len, hcns(bool) eof)
{
	int version;
	int index;
	ei_term e[1];
	char *str;
	int r;

	assert(eof == 1);  /* eof is expected since the buffer is
			    * sufficiently large and in erlang's
			    * distribution protocol we now the buffer
			    * size in advance */

	index = 0;

	assert(ei_decode_version(buf, &index, &version) == 0);
	assert(version == 131 /* 130 in erlang 4.2 */);
	
	int safeloop = 100;

	while (index < len && safeloop) {
		int prior = index;
		int ttype, tlen;

		ttype = -1;
		tlen = -1;

		assert(ei_get_type(buf, &index, &ttype, &tlen) == 0);
		if (ttype == 70) {
			hcns(u4) u4u4[2];
			int endiancheck = 0xdeadbeef;

			/* unsupported type by erl_interface
			 */
			index += sizeof(e->ei_type);

			switch (ttype) {
			case 70: /* public static final int newFloatTag = 70: from otp_src_R13B04/lib/jinterface/java_src/com/ericsson/otp/erlang/OtpExternal.java */

				assert(*((unsigned char*)&endiancheck) == 0xef);  /* little endian only so far */

				u4u4[1] = HC_GET_BE4(buf + index);
				u4u4[0] = HC_GET_BE4(buf + index + 4);

				e->value.d_val = (double) *((double*)u4u4);
				index += sizeof(double);

				fprintf(stderr, "TYPE 70: %f\n", e->value.d_val);
				break;
			default:
				HC_FATAL("unsupported type: %i", ttype);
			}
		} else {
			e->ei_type = -1;
			e->arity = -1;
			e->size = -1;

			r = ei_decode_ei_term(buf, &index, e);
			if (r == -1) {
				HC_FATAL("failed to decode term, index: %i (type %i)", index, e->ei_type);
			}
			assert(r == 0 || r == 1);
			assert(r == 0 || prior != index);  /* when r == 1, buffer was consumed */

			switch (e->ei_type) {
			case ERL_SMALL_INTEGER_EXT:
				if (HC_ISGRAPH(e->value.i_val)) {
					fprintf(stderr, "ERL_SMALL_INTEGER_EXT: %li [%c] [a: %i]\n",
						e->value.i_val, (unsigned char)e->value.i_val, e->arity);
				} else {
					fprintf(stderr, "ERL_SMALL_INTEGER_EXT: %li\n", e->value.i_val);
				}
				break;
			case ERL_STRING_EXT:
				/* int ei_decode_string(const char *buf, int *index, char *p)
				 */
				HC_ALLOC(str, e->size + 1);
				assert(ei_decode_string(buf, &index, str) == 0);
				str[e->size] = '\0';
				fprintf(stderr, "ERL_STRING_EXT: [%s] [a: %i]\n", str, e->arity);
				HC_FREE(str);
				break;
			case ERL_INTEGER_EXT:
				fprintf(stderr, "ERL_INTEGER_EXT: %li\n", (long)e->value.i_val);
				break;
			case ERL_FLOAT_EXT:
				fprintf(stderr, "ERL_FLOAT_EXT: %f\n", e->value.d_val);
				break;
			case ERL_ATOM_EXT:
			case ERL_REFERENCE_EXT:
			case ERL_NEW_REFERENCE_EXT:
			case ERL_PORT_EXT:
			case ERL_PID_EXT:
			case ERL_SMALL_TUPLE_EXT:
			case ERL_LARGE_TUPLE_EXT:
			case ERL_NIL_EXT:
			case ERL_BINARY_EXT:
			case ERL_SMALL_BIG_EXT:
			case ERL_LARGE_BIG_EXT:
			case ERL_PASS_THROUGH:
			case ERL_LIST_EXT:
			case ERL_NEW_CACHE:
			case ERL_CACHED_ATOM:
				fprintf(stderr, "unimplemented, skipping: i=%i (was %i), r=%i, t=%i (%c), a=%i, s=%i\n",
					index, prior, r, e->ei_type, e->ei_type, e->arity, e->size);
				ei_skip_term(buf, &index);
				break;
			default:
				fprintf(stderr, "unknown type: i=%i (was %i), r=%i, t=%i (%c), a=%i, s=%i\n",
					index, prior, r, e->ei_type, e->ei_type, e->arity, e->size);
				continue;
			}
		}

		safeloop--;
	}

	fprintf(stderr, "processed %i bytes\n", len);
	return len;
}

int main(int argc, char **argv)
{
	int bufsz = 1024 * 1024;   /* enough size to read any practical term */
	void *buf;
	int fd;

	HC_ALLOC(buf, bufsz);

	fprintf(stderr, "reading %s\n", "t2b.escript.bin");
	fd = open("t2b.escript.bin", O_RDONLY);
	fprintf(stderr, "total bytes read: %i\n", hcns(readfd)(fd, buf, bufsz, doit));
	close(fd);

	fprintf(stderr, "reading %s\n", "t2b.c.bin");
	fd = open("t2b.c.bin", O_RDONLY);
	fprintf(stderr, "total bytes read: %i\n", hcns(readfd)(fd, buf, bufsz, doit));
	close(fd);

	HC_FREE(buf);

	return 0;
}
