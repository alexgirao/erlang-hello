/*
 * ./test 2>test.stderr | hexdump -C
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
#include "ei.h"

#include "common-inc.c"

int main(int argc, char **argv)
{
	HC_DEF_S(buf);

	ei_sanity_check();

	encode_version(buf);
	encode_list_header(buf, 3);
	encode_atomz(buf, "c");
	encode_atomz(buf, "d");
	encode_list_header(buf, 1);
	encode_atomz(buf, "e");
	encode_atomz(buf, "f");
	encode_empty_list(buf);

	fprintf(stderr, "------------------------------ %i\n", buf->len);

	hcns(write_exact)(1, buf->s, buf->len); /* beware, non-ascii output, intended to be used with hexdump -C */

	hcns(s_free)(buf);

	return 0;
}
