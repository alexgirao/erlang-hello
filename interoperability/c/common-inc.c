
// otp_src_R13B04/lib/erl_interface/src/misc/ei_decode_term.c

/* do some "preflight"s to ensure correct term sizes
 */

void ei_sanity_check()
{
	int index;

	index = 0;
	assert(ei_encode_version(NULL, &index) == 0);
	assert(index == 1);

	fprintf(stderr, "all ei sanity checks passed\n");
}

void encode_version(HC_ST_S *x)
{
	if (x->len == 0) {
		hcns(s_alloc)(x, 100);
	} else {
		hcns(s_alloc)(x, x->len + 1);
	}
	assert(ei_encode_version(x->s, &x->len) == 0);
}

void encode_list_header(HC_ST_S *x, int n)
{
	int i = x->len;
	ei_encode_list_header(NULL, &i, n);
	hcns(s_alloc)(x, i);
	assert(ei_encode_list_header(x->s, &x->len, n) == 0);
}

void encode_empty_list(HC_ST_S *x)
{
	int i = x->len;
	ei_encode_empty_list(NULL, &i);
	hcns(s_alloc)(x, i);
	ei_encode_empty_list(x->s, &x->len);
}

void encode_atomn(HC_ST_S *x, const char* s, int len)
{
	int i = x->len;
	ei_encode_atom_len(NULL, &i, s, len);
	hcns(s_alloc)(x, i);
	assert(ei_encode_atom_len(x->s, &x->len, s, len) == 0);
}

void encode_atomz(HC_ST_S *x, const char* s)
{
	encode_atomn(x, s, hcns(slen)(s));
}
