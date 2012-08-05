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

#include "erl_interface.h"
#include "ei.h"

#include "str.h"
#include "item.h"

#include "eterm.h"

#if 0
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

int main(int argc, char **argv)
{
	DEFINE_STR(buf);

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

	write_exact(1, buf->s, buf->len); /* beware, non-ascii output, intended to be used with hexdump -C */

	str_free(buf);

	return 0;
}
