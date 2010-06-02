
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
	if (x->a == 0) {
		hcns(s_alloc)(x, 100);
	} else {
		hcns(s_alloc)(x, x->len + 1);
	}
	assert(ei_encode_version(x->s, &x->len) == 0);
}

void encode_char(HC_ST_S *x, char p)
{
	int i = x->len;
	ei_encode_char(NULL, &i, p);
	hcns(s_alloc)(x, i);
	ei_encode_char(x->s, &x->len, p);
}

void encode_boolean(HC_ST_S *x, int p)
{
	int i = x->len;
	ei_encode_boolean(NULL, &i, p);
	hcns(s_alloc)(x, i);
	ei_encode_boolean(x->s, &x->len, p);
}

void encode_double(HC_ST_S *x, double dbl)
{
	int i = x->len;
	ei_encode_double(NULL, &i, dbl);
	hcns(s_alloc)(x, i);
	ei_encode_double(x->s, &x->len, dbl);
}

void _encode_double_type70(char *buf, int *index, double p)
{
	char *s = buf + *index;
	char *s0 = s;
	hcns(u4) *q = (hcns(u4)*) &p;
	int endiancheck = 0xdeadbeef;

	assert(*((unsigned char*)&endiancheck) == 0xef);  /* little endian only so far, FIXME: automate this with preprocessor */

	if (!buf) s++;
	else {
		*s++ = 70;
		HC_PUT_BE4(s + 4, *q);
		HC_PUT_BE4(s, *(q + 1));
	}
	s += sizeof(double);

	*index += s - s0;
}

void encode_double_type70(HC_ST_S *x, double dbl)
{
	int i = x->len;
	_encode_double_type70(NULL, &i, dbl);
	hcns(s_alloc)(x, i);
	_encode_double_type70(x->s, &x->len, dbl);
}

void encode_long(HC_ST_S *x, long n)
{
	int i = x->len;
	ei_encode_long(NULL, &i, n);
	hcns(s_alloc)(x, i);
	ei_encode_long(x->s, &x->len, n);
}

void encode_ulong(HC_ST_S *x, unsigned long n)
{
	int i = x->len;
	ei_encode_ulong(NULL, &i, n);
	hcns(s_alloc)(x, i);
	ei_encode_ulong(x->s, &x->len, n);
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

void encode_stringn(HC_ST_S *x, const char* s, int len)
{
	int i = x->len;
	ei_encode_string_len(NULL, &i, s, len);
	hcns(s_alloc)(x, i);
	ei_encode_string_len(x->s, &x->len, s, len);
}

void encode_stringz(HC_ST_S *x, const char* s)
{
	encode_stringn(x, s, hcns(slen)(s));
}

void encode_tuple_header(HC_ST_S *x, long n)
{
	int i = x->len;
	ei_encode_tuple_header(NULL, &i, n);
	hcns(s_alloc)(x, i);
	ei_encode_tuple_header(x->s, &x->len, n);
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
