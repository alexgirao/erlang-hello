#ifndef tid020767f98837ddfh4l4jukm8egiy3ucfv7oi2lc0v5k /* eterm-h */
#define tid020767f98837ddfh4l4jukm8egiy3ucfv7oi2lc0v5k /* eterm-h */

#define ERL_TINY_ATOM_EXT (ERL_ATOM_EXT + 1000)      /* ERL_ATOM_EXT that has at most sizeof(HC_ST_S)-1 bytes */
#define ERL_TINY_STRING_EXT (ERL_STRING_EXT + 1000)  /* ERL_STRING_EXT that has at most sizeof(HC_ST_S)-1 bytes */
#define ERL_TINY_TYPE_MAXLEN (sizeof(HC_ST_S)-1)

HC_DECL_PUBLIC_I_HEADERS(eterm,
		  int type;
		  int len;  /* composite type arity, string/atom len */
		  union {
			  long i_val;
			  double d_val;
			  struct eterm *children;  /* composite type children */
			  HC_ST_S str[1]; /* holds ERL_STRING_EXT / ERL_ATOM_EXT data, 0 terminated */
			  char tinystr[sizeof(HC_ST_S)]; /* ERL_TINY_ATOM_EXT, ERL_TINY_STRING_EXT */
		  } value;
	);

void ei_sanity_check();

void encode_version(HC_ST_S *x);
void encode_char(HC_ST_S *x, char p);
void encode_boolean(HC_ST_S *x, int p);
void encode_double(HC_ST_S *x, double dbl);
void encode_double_type70(HC_ST_S *x, double dbl);
void encode_long(HC_ST_S *x, long n);
void encode_ulong(HC_ST_S *x, unsigned long n);
void encode_atomn(HC_ST_S *x, const char* s, int len);
void encode_atomz(HC_ST_S *x, const char* s);
void encode_stringn(HC_ST_S *x, const char* s, int len);
void encode_stringz(HC_ST_S *x, const char* s);
void encode_tuple_header(HC_ST_S *x, long n);
void encode_list_header(HC_ST_S *x, int n);
void encode_empty_list(HC_ST_S *x);

int decode_double_type70(const char *buf, int *index, double *p);

int skip_term(const char* buf, int *index);

struct eterm *eterm_decode(const char *buf, int len, int *index);
void eterm_free(struct eterm *h);
void eterm_show(int level, struct eterm *h);

#endif
