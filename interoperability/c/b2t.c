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

// todo: do not use ei_term (too large structure, 288 bytes on arch32)

#include "common-inc.c"

/* processing function, return amount of successfully processed data
 */
static struct eterm *doit2(const char *buf, int len, int *index, int depth, struct eterm *parent, struct eterm *h)
{
	int n = parent ? parent->len : -1;
	int type, tlen;

	while (*index < len && n--) {
		h = eterm_new0(h);

		assert(ei_get_type(buf, index, &type, &tlen) == 0);
		h->type = type;
		if (type == 70) {
			/* unsupported type by erl_interface
			 */
			switch (type) {
			case 70: /* IEEE 754 */
				decode_double_type70(buf, index, &h->value.d_val);
				break;
			default:
				HC_FATAL("unsupported type: %i", type);
			}
		} else {
			h->type = type;
			h->len = tlen;

			switch (type) {
			case ERL_SMALL_INTEGER_EXT:
			case ERL_INTEGER_EXT:
				assert(ei_decode_long(buf, index, &h->value.i_val) == 0);
				break;
			case ERL_ATOM_EXT:
				hcns(s_alloc)(h->value.str, tlen + 1);  /* need extra byte for '\0', as manual says */
				assert(ei_decode_atom(buf, index, h->value.str->s) == 0);
				h->value.str->len = tlen;
				break;
			case ERL_FLOAT_EXT:
				assert(ei_decode_double(buf, index, &h->value.d_val) == 0);
				break;
			case ERL_STRING_EXT:
				hcns(s_alloc)(h->value.str, tlen + 1);  /* need extra byte for '\0', as manual says */
				assert(ei_decode_string(buf, index, h->value.str->s) == 0);
				h->value.str->len = tlen;
				break;
			case ERL_SMALL_TUPLE_EXT:
			case ERL_LARGE_TUPLE_EXT:
				assert(ei_decode_tuple_header(buf, index, NULL) == 0); /* skip tuple header */
				h->value.children = doit2(buf, len, index, depth + 1, h, NULL);
				break;
			case ERL_LIST_EXT:
				assert(ei_decode_list_header(buf, index, NULL) == 0); /* skip list header */
				h->value.children = doit2(buf, len, index, depth + 1, h, NULL);
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
				assert(skip_term(buf, index) == 0);
				break;
			case ERL_NIL_EXT:
				assert(ei_decode_list_header(buf, index, NULL) == 0);
				assert(*index == len || depth > 0); /* top level NIL must be at end of buffer */
				break;
			default:
				fprintf(stderr, "unknown type: i=%i, t=%i (%c), l=%i\n",
					*index, h->type, h->type, tlen);
				continue;
			}
		}
	}

	if (parent && parent->type == ERL_LIST_EXT) {
		/* list terms expects a tail item (any other term)
		 */
		struct eterm tmp[1];

		assert(ei_get_type(buf, index, &type, &tlen) == 0);

		if (type == ERL_LIST_EXT) {
			assert(ei_decode_list_header(buf, index, NULL) == 0); /* skip list header */

			tmp->type = ERL_LIST_EXT;
			tmp->len = tlen;

			h = doit2(buf, len, index, depth + 1, tmp, h); /* decode tail list as sibling through ``tmp'' */

			parent->len += tmp->len; /* ``tmp'' gets updated in inner calls */
		} else if (type == ERL_STRING_EXT) {
			unsigned char *str;
			HC_ALLOC(str, tlen + 1); /* need extra byte for '\0', as manual says */

			assert(ei_decode_string(buf, index, (char *)str) == 0);

			for (n=0; n<tlen; n++) {
				h = eterm_new0(h);
				h->type = ERL_SMALL_INTEGER_EXT;
				h->value.i_val = str[n];
			}

			h = eterm_new0(h);
			h->type = ERL_NIL_EXT;

			parent->len += tlen;

			HC_FREE(str);
		} else {
			h = doit2(buf, len, index, depth + 1, NULL, h);
		}
	}

	return h;
}

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

	h = doit2(buf, len, &index, 0, NULL, NULL);

	fflush(stderr);
	fflush(stdout);

	eterm_show(0, h);

	fflush(stderr);
	fflush(stdout);

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

	printf("sizeof(struct eterm) = %i\n", sizeof(struct eterm));

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
