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
	encode_long(buf, 1);
	encode_double(buf, 1.618034);
	encode_atomz(buf, "a_atom");
	encode_stringz(buf, "a_string");
	encode_boolean(buf, 1);
	encode_boolean(buf, 0);

	/* {a_atom, 1, 1.618034, "a_string"}
	 */
	encode_tuple_header(buf, 4);
	encode_atomz(buf, "a_atom");
	encode_char(buf, 1);
	encode_double(buf, 1.618034);
	encode_stringz(buf, "a_string");

	/* [a_list, 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144]
	 * encoded in canonical form
	 */
	encode_list_header(buf, 1);
	encode_atomz(buf, "a_list");
	encode_list_header(buf, 1);
	encode_char(buf, 0);
	encode_list_header(buf, 1);
	encode_char(buf, 1);
	encode_list_header(buf, 1);
	encode_char(buf, 1);
	encode_list_header(buf, 1);
	encode_char(buf, 2);
	encode_list_header(buf, 1);
	encode_char(buf, 3);
	encode_list_header(buf, 1);
	encode_char(buf, 5);
	encode_list_header(buf, 1);
	encode_char(buf, 8);
	encode_list_header(buf, 1);
	encode_char(buf, 13);
	encode_list_header(buf, 1);
	encode_char(buf, 21);
	encode_list_header(buf, 1);
	encode_char(buf, 34);
	encode_list_header(buf, 1);
	encode_char(buf, 55);
	encode_list_header(buf, 1);
	encode_char(buf, 89);
	encode_list_header(buf, 1);
	encode_char(buf, 144);
	encode_empty_list(buf);

	encode_empty_list(buf);

	hcns(write_exact)(1, buf->s, buf->len); /* beware, non-ascii output, intended to be used with hexdump -C */

	hcns(s_free)(buf);

	return 0;
}
