#ifndef tid020767f98837ddfh4l4jukm8egiy3ucfv7oi2lc0v5k /* eterm-h */
#define tid020767f98837ddfh4l4jukm8egiy3ucfv7oi2lc0v5k /* eterm-h */

#define ERL_TINY_ATOM_EXT (ERL_ATOM_EXT + 1000)      /* ERL_ATOM_EXT that has at most sizeof(struct str)-1 bytes */
#define ERL_TINY_STRING_EXT (ERL_STRING_EXT + 1000)  /* ERL_STRING_EXT that has at most sizeof(struct str)-1 bytes */
#define ERL_TINY_BINARY_EXT (ERL_BINARY_EXT + 1000)
#define ERL_TINY_TYPE_MAXLEN (sizeof(struct str)-1)
#define ERL_TINY_BINARY_MAXLEN (sizeof(struct str)) /* binary doesn't have '\0' */

DEFINE_ITEM_HEADER(eterm,
		  int type;
		  int len;  /* composite type arity, string/atom len */
		  union {
			  long i_val;
			  double d_val;
			  struct eterm *children;  /* composite type children */
			  struct str str[1]; /* holds ERL_STRING_EXT / ERL_ATOM_EXT data, 0 terminated */
			  char tinystr[sizeof(struct str)]; /* ERL_TINY_ATOM_EXT, ERL_TINY_STRING_EXT */
		  } value;
	);

void ei_sanity_check();

void encode_version(struct str *x);
void encode_char(struct str *x, char p);
void encode_boolean(struct str *x, int p);
void encode_double(struct str *x, double dbl);
void encode_double_type70(struct str *x, double dbl);
void encode_long(struct str *x, long n);
void encode_ulong(struct str *x, unsigned long n);
void encode_atomn(struct str *x, const char* s, int len);
void encode_atomz(struct str *x, const char* s);
void encode_stringn(struct str *x, const char* s, int len);
void encode_stringz(struct str *x, const char* s);
void encode_binary(struct str *x, void *s, int len);
void encode_tuple_header(struct str *x, long n);
void encode_list_header(struct str *x, int n);
void encode_empty_list(struct str *x);

int decode_double_type70(const char *buf, int *index, double *p);

int skip_term(const char* buf, int *index);

struct eterm *eterm_decode(const char *buf, int len, int *index);
void eterm_free(struct eterm *h);
void eterm_show(int level, struct eterm *h);

#endif
