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
#include <higherc/s.h>

#include "erl_interface.h"
#include "ei.h"

// otp_src_R13B04/lib/erl_interface/src/misc/ei_decode_term.c



int main(int argc, char **argv)
{
	HC_DEF_S(buf);

	hcns(s_alloc)(buf, 100);

	//hcns(s_free)(buf);

	return 0;
}
