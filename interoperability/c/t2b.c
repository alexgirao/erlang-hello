/*
 * ./t2b 2>t2b.stderr | hexdump -C
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

	/*

        [
	 1,
	 1.618034,
	 a_atom,
	 "a_string",
	 true,
	 false,
	 {a_atom, 1, 1.618034, "a_string"},
	 [a_list, 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144]
	]

	 */

	ei_sanity_check();

	encode_version(buf);

	encode_list_header(buf, 8);
	//encode_
	encode_empty_list(buf);

	fprintf(stderr, "------------------------------ %i\n", buf->len);

	hcns(write_exact)(1, buf->s, buf->len); /* beware, non-ascii output, intended to be used with hexdump -C */

	hcns(s_free)(buf);

	return 0;
}
