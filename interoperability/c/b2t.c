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
static struct eterm *doit2(const char *buf, int len, int *index, int depth, struct eterm *parent)
{
	ei_term *e;
	int r;
	struct eterm *h = NULL;
	int safeloop = 100;
	/* int skipindex; */
	int n = parent ? parent->t->arity : -1;

	while (*index < len && n-- && safeloop) {
		int prior = *index;
		int ttype, tlen;

		h = eterm_new0(h);
		e = h->t;

		ttype = -1;
		tlen = -1;

		assert(ei_get_type(buf, index, &ttype, &tlen) == 0);
		e->ei_type = ttype;
		if (ttype == 70) {
			/* unsupported type by erl_interface
			 */
			switch (ttype) {
			case 70: /* EEE 754 */
				decode_double_type70(buf, index, &e->value.d_val);
				fprintf(stderr, "TYPE 70: %f\n", e->value.d_val);
				break;
			default:
				HC_FATAL("unsupported type: %i", ttype);
			}
		} else {
			e->ei_type = -1;
			e->arity = -1;
			e->size = -1;

			r = ei_decode_ei_term(buf, index, e);
			if (r == -1) {
				HC_FATAL("failed to decode term, *index: %i (type %i)", *index, e->ei_type);
			}
			assert(r == 0 || r == 1);
			assert(r == 0 || prior != *index);  /* when r == 1, buffer was consumed */

			switch (e->ei_type) {
			case ERL_SMALL_INTEGER_EXT:
			case ERL_ATOM_EXT:
			case ERL_INTEGER_EXT:
			case ERL_FLOAT_EXT:
				/* simple types
				 */
				assert(r == 1);
				break;
			case ERL_STRING_EXT:
				hcns(s_alloc)(h->str, e->size + 1);  /* as manual says */
				assert(ei_decode_string(buf, index, h->str->s) == 0);
				break;
			case ERL_SMALL_TUPLE_EXT:
			case ERL_LARGE_TUPLE_EXT:
			case ERL_LIST_EXT:
				/* compound types
				 */
				assert(r == 1);
				h->children = doit2(buf, len, index, depth + 1, h);
				break;
			case ERL_REFERENCE_EXT:
			case ERL_NEW_REFERENCE_EXT:
			case ERL_PORT_EXT:
			case ERL_PID_EXT:
			case ERL_BINARY_EXT:
			case ERL_SMALL_BIG_EXT:
			case ERL_LARGE_BIG_EXT:
			case ERL_PASS_THROUGH:
			case ERL_NEW_CACHE:
			case ERL_CACHED_ATOM:
				fprintf(stderr, "unimplemented, skipping: i=%i (was %i), r=%i, t=%i (%c), a=%i, s=%i\n",
					*index, prior, r, e->ei_type, e->ei_type, e->arity, e->size);
				*index = prior; /* rewind term to skip */
				//ei_skip_term(buf, index);
				assert(skip_term(buf, index) == 0);
				fprintf(stderr, "index after skip: %i\n", *index);
				break;
			case ERL_NIL_EXT:
				assert(ei_decode_list_header(buf, index, NULL) == 0);
				assert(*index == len || depth > 0);
				break;
			default:
				fprintf(stderr, "unknown type: i=%i (was %i), r=%i, t=%i (%c), a=%i, s=%i\n",
					*index, prior, r, e->ei_type, e->ei_type, e->arity, e->size);
				continue;
			}
		}

		safeloop--;
	}

	return h;
}

static void eterm_show(int level, struct eterm *h)
{
	struct eterm_iter i[1];

	eterm_forward(h, i);
	while ((h = eterm_next(i))) {
		printf("%i: %p: %i\n", level, h, h->t->ei_type);
		if (h->children) {
			eterm_show(level + 1, h->children);
		}
	}
	eterm_end(i);
}

static int doit(const char *buf, int len, hcns(bool) eof)
{
	struct eterm *h = NULL;
	int index = 0;
	int version;

	assert(eof == 1);  /* eof is expected since the buffer is
			    * sufficiently large and in erlang's
			    * distribution protocol we now the buffer
			    * size in advance */


	assert(ei_decode_version(buf, &index, &version) == 0);
	assert(version == 131 /* 130 in erlang 4.2 */);

	h = doit2(buf, len, &index, 0, h);

	eterm_show(0, h);

	/* clean up
	 */

	eterm_free0(h);

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

#if 0
	fprintf(stderr, "reading %s\n", "t2b.c.bin");
	fd = open("t2b.c.bin", O_RDONLY);
	fprintf(stderr, "total bytes read: %i\n", hcns(readfd)(fd, buf, bufsz, doit));
	close(fd);
#endif

	HC_FREE(buf);

	return 0;
}
