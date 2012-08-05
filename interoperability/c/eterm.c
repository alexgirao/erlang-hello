#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>  /* /usr/include/asm-generic/errno.h */
#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <ctype.h>
#include <stdint.h>
#include <stdarg.h>

#include "erl_interface.h"

#include "str.h"
#include "item.h"

#include "eterm.h"

DEFINE_ITEM_IMPLEMENTATION(eterm);

#define HEX_DIGIT(v) ((v) >= 0 && (v) < 16 ? "0123456789abcdef"[v] : '?')

#if defined (__STDC_VERSION__) && __STDC_VERSION__ >= 199901L
# define DIE(...)	    _p_fatal(__FILE__, __LINE__, __VA_ARGS__)
#elif defined (__GNUC__)
# define DIE(fmt...)   _p_fatal(__FILE__, __LINE__, fmt)
#endif

static void _p_fatal(char *file, int line, char *fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	fprintf(stderr, "fatal-error: %s: %i: ", file, (int)line);
	vfprintf(stderr, fmt, args);
	fprintf(stderr, "\n");
	fflush(stderr);
	va_end(args);
	exit(1);
}

static void s_reprn(struct str *s_repr, void *s, int n)
{
	unsigned char *x = s;
	int c;
	while (n--) {
		c = *x++;
		if (isprint(c)) {
			if (c == '\t') {
				str_alloc(s_repr, s_repr->len + 2); /* \t */
				s_repr->s[s_repr->len++] = '\\';
				s_repr->s[s_repr->len++] = 't';
			} else {
				str_catc(s_repr, c);
			}
		} else {
			str_alloc(s_repr, s_repr->len + 4); /* \xNN */
			s_repr->s[s_repr->len++] = '\\';
			s_repr->s[s_repr->len++] = 'x';
			s_repr->s[s_repr->len++] = HEX_DIGIT((c >> 4) & 0xf);
			s_repr->s[s_repr->len++] = HEX_DIGIT(c & 0xf);
		}
	}
}

#if 0
static void s_repr(struct str *s_repr, struct str *s)
{
	s_reprn(s_repr, s->s, s->len);
}

static void s_reprz(struct str *s_repr, char *s)
{
	s_reprn(s_repr, s, strlen(s));
}
#endif

static void print_s_reprn(char *prefix, char *s, int len, char *suffix)
{
	DEFINE_STR(r);
	s_reprn(r, s, len);
	if (prefix) printf("%s", prefix);
	fwrite(r->s, r->len, 1, stdout);
	if (suffix) printf("%s", suffix);
	str_free(r);
}

static void print_s_repr(char *prefix, struct str *s, char *suffix)
{
	print_s_reprn(prefix, s->s, s->len, suffix);
}

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

void encode_version(struct str *x)
{
	if (x->a == 0) {
		str_alloc(x, 100);
	} else {
		str_alloc(x, x->len + 1);
	}
	assert(ei_encode_version(x->s, &x->len) == 0);
}

void encode_char(struct str *x, char p)
{
	int i = x->len;
	ei_encode_char(NULL, &i, p);
	str_alloc(x, i);
	ei_encode_char(x->s, &x->len, p);
}

void encode_boolean(struct str *x, int p)
{
	int i = x->len;
	ei_encode_boolean(NULL, &i, p);
	str_alloc(x, i);
	ei_encode_boolean(x->s, &x->len, p);
}

void encode_double(struct str *x, double dbl)
{
	int i = x->len;
	ei_encode_double(NULL, &i, dbl);
	str_alloc(x, i);
	ei_encode_double(x->s, &x->len, dbl);
}

static void _encode_double_type70(char *buf, int *index, double p)
{
	char *s = buf + *index;
	char *s0 = s;
	uint32_t *q = (uint32_t*) &p;

	if (!buf) s++;
	else {
		/* assume little-endian double
		 */
		*s++ = 70;
		*(uint32_t*)(s + 4) = htonl(*q);
		*(uint32_t*)s = htonl(*(q + 1));
	}
	s += sizeof(double);

	*index += s - s0;
}

void encode_double_type70(struct str *x, double dbl)
{
	int i = x->len;
	_encode_double_type70(NULL, &i, dbl);
	str_alloc(x, i);
	_encode_double_type70(x->s, &x->len, dbl);
}

void encode_long(struct str *x, long n)
{
	int i = x->len;
	ei_encode_long(NULL, &i, n);
	str_alloc(x, i);
	ei_encode_long(x->s, &x->len, n);
}

void encode_ulong(struct str *x, unsigned long n)
{
	int i = x->len;
	ei_encode_ulong(NULL, &i, n);
	str_alloc(x, i);
	ei_encode_ulong(x->s, &x->len, n);
}

void encode_atomn(struct str *x, const char* s, int len)
{
	int i = x->len;
	ei_encode_atom_len(NULL, &i, s, len);
	str_alloc(x, i);
	assert(ei_encode_atom_len(x->s, &x->len, s, len) == 0);
}

void encode_atomz(struct str *x, const char* s)
{
	encode_atomn(x, s, strlen(s));
}

void encode_stringn(struct str *x, const char* s, int len)
{
	int i = x->len;
	ei_encode_string_len(NULL, &i, s, len);
	str_alloc(x, i);
	ei_encode_string_len(x->s, &x->len, s, len);
}

void encode_stringz(struct str *x, const char* s)
{
	encode_stringn(x, s, strlen(s));
}

void encode_binary(struct str *x, void *s, int len)
{
	int i = x->len;
	ei_encode_binary(NULL, &i, s, len);
	str_alloc(x, i);
	ei_encode_binary(x->s, &x->len, s, len);
}

void encode_tuple_header(struct str *x, long n)
{
	int i = x->len;
	ei_encode_tuple_header(NULL, &i, n);
	str_alloc(x, i);
	ei_encode_tuple_header(x->s, &x->len, n);
}

void encode_list_header(struct str *x, int n)
{
	int i = x->len;
	ei_encode_list_header(NULL, &i, n);
	str_alloc(x, i);
	assert(ei_encode_list_header(x->s, &x->len, n) == 0);
}

void encode_empty_list(struct str *x)
{
	int i = x->len;
	ei_encode_empty_list(NULL, &i);
	str_alloc(x, i);
	ei_encode_empty_list(x->s, &x->len);
}

int decode_double_type70(const char *buf, int *index, double *p)
{
	const char *s = buf + *index;
	const char *s0 = s;
	uint32_t u4u4[2];

	if (*s++ != 70) return -1;

	/* assume little-endian double
	 */
	u4u4[1] = ntohl(*(uint32_t*)s);
	u4u4[0] = ntohl(*(uint32_t*)(s + 4));
	
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

void eterm_free(struct eterm *h)
{
	struct eterm_iterator i[1];
	struct eterm *t;

	eterm_backward(i, h);
	while ((t = eterm_next(i))) {
		switch (t->type) {
		case ERL_SMALL_TUPLE_EXT:
		case ERL_LARGE_TUPLE_EXT:
		case ERL_LIST_EXT:
			assert(t->value.children != NULL);
			if (t->value.children) {
				eterm_free(t->value.children);
				t->value.children = NULL;
			}
			break;
		case ERL_ATOM_EXT:
		case ERL_STRING_EXT:
		case ERL_BINARY_EXT:
			str_free(t->value.str);
			break;
		}
	}
	eterm_end(i);

	eterm_free0(h);
}

void eterm_show(int level, struct eterm *h)
{
	struct eterm_iterator i[1];
	int count = 1, n;

	eterm_forward(i, h);
	while ((h = eterm_next(i))) {
		for (n=0; n < level; n++) {
			fwrite("  ", 2, 1, stdout);
		}

		switch (h->type) {
		case ERL_SMALL_INTEGER_EXT:
		case ERL_INTEGER_EXT:
			printf("%i: [%i]: %p: %i %li\n", level, count, h, h->type, h->value.i_val);
			break;
		case ERL_ATOM_EXT:
		case ERL_STRING_EXT:
		case ERL_BINARY_EXT:
			printf("%i: [%i]: %p: %.4i [", level, count, h, h->type);
			print_s_repr(NULL, h->value.str, "]\n");
			break;
		case ERL_TINY_ATOM_EXT:
		case ERL_TINY_STRING_EXT:
		case ERL_TINY_BINARY_EXT:
			printf("%i: [%i]: %p: %.4i [", level, count, h, h->type);
			print_s_reprn(NULL, h->value.tinystr, h->len, "]\n");
			break;
		case 70: /* newFloatTag */
		case ERL_FLOAT_EXT:
			printf("%i: [%i]: %p: %i %f\n", level, count, h, h->type, h->value.d_val);
			break;
		case ERL_SMALL_TUPLE_EXT:
		case ERL_LARGE_TUPLE_EXT:
		case ERL_LIST_EXT:
			printf("%i: [%i]: %p: %i %i items\n", level, count, h, h->type, h->len);
			eterm_show(level + 1, h->value.children);
			break;
		default:
			printf("%i: [%i]: %p: %i\n", level, count, h, h->type);
		}
		count++;
	}
	eterm_end(i);
}

/* assumes, h->type and h->len are already set, doesn't decode
 * composite terms or nil
 */
static void decode1term(const char *buf, int *index, struct eterm *h)
{
	switch (h->type) {
	case ERL_SMALL_INTEGER_EXT:
	case ERL_INTEGER_EXT:
		assert(ei_decode_long(buf, index, &h->value.i_val) == 0);
		break;
	case 70: /* IEEE 754 */
		assert(decode_double_type70(buf, index, &h->value.d_val) == 0);
		break;
	case ERL_FLOAT_EXT:
		assert(ei_decode_double(buf, index, &h->value.d_val) == 0);
		break;
	case ERL_ATOM_EXT:
		if (h->len <= ERL_TINY_TYPE_MAXLEN) {
			h->type = ERL_TINY_ATOM_EXT;
			assert(ei_decode_atom(buf, index, h->value.tinystr) == 0);
		} else {
			str_alloc(h->value.str, h->len + 1);  /* need extra byte for '\0', as manual says */
			assert(ei_decode_atom(buf, index, h->value.str->s) == 0);
			h->value.str->len = h->len;
		}
		break;
	case ERL_STRING_EXT:
		if (h->len <= ERL_TINY_TYPE_MAXLEN) {
			h->type = ERL_TINY_STRING_EXT;
			assert(ei_decode_string(buf, index, h->value.tinystr) == 0);
		} else {
			str_alloc(h->value.str, h->len + 1);  /* need extra byte for '\0', as manual says */
			assert(ei_decode_string(buf, index, h->value.str->s) == 0);
			h->value.str->len = h->len;
		}
		break;
	case ERL_BINARY_EXT:
		if (h->len <= ERL_TINY_BINARY_MAXLEN) {
			h->type = ERL_TINY_BINARY_EXT;
			assert(ei_decode_binary(buf, index, h->value.tinystr, NULL) == 0);
		} else {
			str_alloc(h->value.str, h->len);
			assert(ei_decode_binary(buf, index, h->value.str->s, NULL) == 0);
			h->value.str->len = h->len;
		}
		break;
	case ERL_REFERENCE_EXT:
	case ERL_NEW_REFERENCE_EXT:
	case ERL_PORT_EXT:
	case ERL_PID_EXT:
	case ERL_SMALL_BIG_EXT:
	case ERL_LARGE_BIG_EXT:
	case ERL_PASS_THROUGH:
	case ERL_NEW_CACHE:
	case ERL_CACHED_ATOM:
		fprintf(stderr, "unimplemented type: i=%i, t=%i (%c), l=%i\n",
			*index, h->type, h->type, h->len);
		assert(skip_term(buf, index) == 0);
		break;
	case ERL_SMALL_TUPLE_EXT:
	case ERL_LARGE_TUPLE_EXT:
	case ERL_LIST_EXT:
	case ERL_NIL_EXT:
		DIE("invalid data, nil not expected %i (%c)", h->type, h->type);
	default:
		DIE("unknown type: i=%i, t=%i (%c), l=%i\n",
			 *index, h->type, h->type, h->len);
	}
}

static struct eterm *eterm_decode0(const char *buf, int len, int *index, int depth, struct eterm *parent, struct eterm *h)
{
	int n = parent ? parent->len : -1;

/* 	printf("debug: entering depth %i\n", depth); */

	while (*index < len && n--) {
		h = eterm_new0(h);
		assert(ei_get_type(buf, index, &h->type, &h->len) == 0);

		switch (h->type) {
		case ERL_SMALL_TUPLE_EXT:
		case ERL_LARGE_TUPLE_EXT:
			assert(ei_decode_tuple_header(buf, index, NULL) == 0); /* skip tuple header */
			h->value.children = eterm_decode0(buf, len, index, depth + 1, h, NULL);
			break;
		case ERL_LIST_EXT:
			assert(ei_decode_list_header(buf, index, NULL) == 0); /* skip list header */
			h->value.children = eterm_decode0(buf, len, index, depth + 1, h, NULL);
			break;
		default:
			decode1term(buf, index, h);
		}

	}

	if (parent && parent->type == ERL_LIST_EXT) {
		/* list terms expects a tail item (any other term)
		 */
		struct eterm tmp[1];
		unsigned char *str = NULL;

		assert(ei_get_type(buf, index, &tmp->type, &tmp->len) == 0);

		switch (tmp->type) {
		case ERL_LIST_EXT:
			assert(ei_decode_list_header(buf, index, NULL) == 0); /* skip list header */
			h = eterm_decode0(buf, len, index, depth, tmp, h); /* decode tail list as sibling through ``tmp'' */
			parent->len += tmp->len; /* ``tmp'' gets updated in inner calls */
			break;
		case ERL_STRING_EXT:
			//HC_ALLOC(str, tmp->len + 1); /* need extra byte for '\0', as manual says */
			str = malloc(tmp->len + 1); /* need extra byte for '\0', as manual says */
			assert(str);

			assert(ei_decode_string(buf, index, (char *)str) == 0);

			for (n=0; n < tmp->len; n++) {
				h = eterm_new0(h);
				h->type = ERL_SMALL_INTEGER_EXT;
				h->value.i_val = str[n];
			}

			h = eterm_new0(h);
			h->type = ERL_NIL_EXT;

			parent->len += tmp->len;

			free(str);
			str = NULL;
			break;
		case ERL_NIL_EXT:
			/* proper list, ends with NIL
			 */
			assert(ei_decode_list_header(buf, index, NULL) == 0); /* skip list header */
			assert(*index == len || depth > 0);

			h = eterm_new0(h);
			h->type = ERL_NIL_EXT;
			break;
		default:
/* 			printf("debug: improper list at %i\n", depth); */
			h = eterm_new0(h);
			h->type = tmp->type;
			h->len = tmp->len;
			decode1term(buf, index, h);
		}
	}

/* 	printf("debug: leaving depth %i\n", depth); */

	return h;
}

struct eterm *eterm_decode(const char *buf, int len, int *index)
{
	return eterm_decode0(buf, len, index, 0, NULL, NULL);
}
