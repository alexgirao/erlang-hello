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

#include "eterm.h"

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

	encode_list_header(buf, 9);
	encode_long(buf, 1);
	encode_double_type70(buf, 1.618034);
	encode_atomz(buf, "a_atom");
	encode_stringz(buf, "a string");
	encode_boolean(buf, 1);
	encode_boolean(buf, 0);

	/* {a_tuple, 1, 1.618034, "a_string"}
	 */
	encode_tuple_header(buf, 4);
	encode_atomz(buf, "a_tuple");
	encode_char(buf, 1);
	encode_double_type70(buf, 1.618034);
	encode_stringz(buf, "another string");

	/* [a_improper_list, [c, d, [e | f]]]
	 */

	encode_list_header(buf, 2);
	  encode_atomz(buf, "a_improper_list");
	  encode_list_header(buf, 3);
	    encode_atomz(buf, "c");
	    encode_atomz(buf, "d");
	    encode_list_header(buf, 1);
	      encode_atomz(buf, "e");
	     encode_atomz(buf, "f");  /* improper list tail */
	   encode_empty_list(buf); /* every proper list ends with an empty list (nil) */	
	 encode_empty_list(buf); /* every proper list ends with an empty list (nil) */	

#if 0
	/* [a_list, 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144]
	 * encoded in canonical form [A | [B | [C | [D | []]]]]
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
	encode_list_header(buf, 1);
	encode_char(buf, 233);
	encode_list_header(buf, 1);
	encode_long(buf, 377);
#else
	/* [a_list, 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144]
	 * encoded in shorthand form (must know number of elements in
	 * advance)
	 */
	encode_list_header(buf, 16);
	encode_atomz(buf, "a_list");
	encode_char(buf, 0);
	encode_char(buf, 1);
	encode_char(buf, 1);
	encode_char(buf, 2);
	encode_char(buf, 3);
	encode_char(buf, 5);
	encode_char(buf, 8);
	encode_char(buf, 13);
	encode_char(buf, 21);
	encode_char(buf, 34);
	encode_char(buf, 55);
	encode_char(buf, 89);
	encode_char(buf, 144);
	encode_char(buf, 233);
	encode_long(buf, 377);
#endif
	encode_empty_list(buf); /* every proper list ends with an empty list (nil) */

	encode_empty_list(buf); /* ends root list */

	hcns(write_exact)(1, buf->s, buf->len); /* beware, non-ascii output, intended to be used with hexdump -C */

	hcns(s_free)(buf);

	return 0;
}
