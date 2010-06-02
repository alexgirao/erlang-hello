
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

int decode_double_type70(const char *buf, int *index, double *p)
{
	const char *s = buf + *index;
	const char *s0 = s;
	int endiancheck = 0xdeadbeef;
	hcns(u4) u4u4[2];

	if (*s++ != 70) return -1;
	
	//if (sscanf(s, "%lf", &f) != 1) return -1;

	assert(*((unsigned char*)&endiancheck) == 0xef);  /* little endian only so far */
	
	u4u4[1] = HC_GET_BE4(s);
	u4u4[0] = HC_GET_BE4(s + 4);
	
	s += sizeof(double);
	if (p) *p = *((double*)u4u4);
	*index += s-s0;
	return 0;
}

int skip_term(const char* buf, int *index)
{
	int i, n, ty;

	/* ASSERT(ep != NULL); */

	ei_get_type_internal(buf, index, &ty, &n);
	switch (ty) {
	case ERL_ATOM_EXT:
		/* FIXME: what if some weird locale is in use? */
		if (ei_decode_atom(buf, index, NULL) < 0) return -1;
		break;
	case ERL_PID_EXT:
		if (ei_decode_pid(buf, index, NULL) < 0) return -1;
		break;
	case ERL_PORT_EXT:
		if (ei_decode_port(buf, index, NULL) < 0) return -1;
		break;
	case ERL_NEW_REFERENCE_EXT:
	case ERL_REFERENCE_EXT:
		if (ei_decode_ref(buf, index, NULL) < 0) return -1;
		break;
	case ERL_NIL_EXT:
		if (ei_decode_list_header(buf, index, &n) < 0) return -1;
		break;
	case ERL_LIST_EXT:
		if (ei_decode_list_header(buf, index, &n) < 0) return -1;
		for (i = 0; i < n; ++i) {
			if (skip_term(buf, index) < 0) {
				return -1;
			}
		}
		if (ei_get_type_internal(buf, index, &ty, &n) < 0) {
			return -1;
		}
		if (ty != ERL_NIL_EXT) {
			if (skip_term(buf, index) < 0) return -1;
		} else if (ei_decode_list_header(buf, index, &n) < 0) {
			return -1;
		}
		break;
	case ERL_STRING_EXT:
		if (ei_decode_string(buf, index, NULL) < 0) return -1;
		break;
	case ERL_SMALL_TUPLE_EXT:
	case ERL_LARGE_TUPLE_EXT:
		if (ei_decode_tuple_header(buf, index, &n) < 0) return -1;
		for (i = 0; i < n; ++i)
			skip_term(buf, index);
		break;
	case ERL_BINARY_EXT:
		if (ei_decode_binary(buf, index, NULL, NULL) < 0)
			return -1;
		break;
	case ERL_SMALL_INTEGER_EXT:
	case ERL_INTEGER_EXT:
		if (ei_decode_long(buf, index, NULL) < 0) return -1;
		break;
	case ERL_SMALL_BIG_EXT:
	case ERL_LARGE_BIG_EXT:
		if (ei_decode_big(buf, index, NULL) < 0) return -1;
		break;
	case ERL_FLOAT_EXT:
		if (ei_decode_double(buf, index, NULL) < 0) return -1;
		break;
	case 70: /* public static final int newFloatTag = 70: from otp_src_R13B04/lib/jinterface/java_src/com/ericsson/otp/erlang/OtpExternal.java */
		if (decode_double_type70(buf, index, NULL) < 0) return -1;
		break;
	case ERL_FUN_EXT:
	case ERL_NEW_FUN_EXT:
		if (ei_decode_fun(buf, index, NULL) < 0) return -1;
		break;
	default:
		return -1;
	}

	return 0;
}

/* highlevel erlang terms
 */

HC_DECL_PRIVATE_I(eterm,
		ei_term t[1]; /* type in t->ei_type */
		struct eterm *children;  /* for lists and tuples, length in t->arity */
		HC_ST_S str[1]; /* ERL_STRING_EXT / aka byte list */
);
