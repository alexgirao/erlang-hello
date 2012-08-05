#ifndef _STR_H_
#define _STR_H_

#include <time.h>

#ifdef __cplusplus
extern "C" { /* assume C declarations for C++ */
#endif

#include <stdarg.h>

#ifdef _MSC_VER
#define __attribute__(x)
#endif

typedef struct str {
  char *s;
  int len; /* can be changed between 0 and a-1 (inclusive) to truncate string */
  int a; /* allocated */
} STRUCT_STR;

#define NULL_STR {NULL, 0, 0}
#define DEFINE_STR(sym) STRUCT_STR sym[1] = {NULL_STR}

void str_alloc(STRUCT_STR *x, int n);
void str_free(STRUCT_STR *x);

void str_copyn(STRUCT_STR *, const char *, int);
void str_copy(STRUCT_STR *, const STRUCT_STR *);
void str_copyz(STRUCT_STR *, const char *);
void str_copyc(STRUCT_STR *sa, int c);

void str_catn(STRUCT_STR *, const char *, int);
void str_cat(STRUCT_STR *, const STRUCT_STR *);
void str_catz(STRUCT_STR *, const char *);
void str_catc(STRUCT_STR *sa, int c);

void str_vformat(STRUCT_STR *sa, int cat, const char *fmt, va_list va);
void str_copyf(STRUCT_STR *sa, const char *fmt, ...) __attribute__ ((format (printf, 2, 3)));
void str_catf(STRUCT_STR *sa, const char *fmt, ...) __attribute__ ((format (printf, 2, 3)));

int str_diffn(STRUCT_STR *a, char *b, int bl);
int str_diff(STRUCT_STR *a, STRUCT_STR *b);
int str_diffz(STRUCT_STR *a, char *b);

void str_upper(STRUCT_STR *s);
void str_lower(STRUCT_STR *s);

int str_len(STRUCT_STR *s);
int str_is_empty(STRUCT_STR *s);

/*
 * str_shiftr:
 *   start .. end range: 0 .. ? (exclusive, -1 allowed for both (means: len - abs(end)))
 * str_shiftl:
 *   start .. end range: 0 .. len (exclusive, -1 allowed for both (means: len - abs(end)))
 * str_shiftr2:
 *   likewise, end is always len + n (always expand)
 * str_shiftl2:
 *   likewise, end is always len (shift from end of string)
 */

void str_shiftr(STRUCT_STR *s, int start, int end, int n, int pad);
void str_shiftl(STRUCT_STR *s, int start, int end, int n, int pad);
void str_shiftr2(STRUCT_STR *s, int start, int n, int pad);
void str_shiftl2(STRUCT_STR *s, int start, int n, int pad);

void str_from_file(STRUCT_STR *s, const char *file);

void str_formattime(STRUCT_STR *sa, int cat, const char *fmt, struct tm *tm);
void str_copyftime(STRUCT_STR *sa, const char *fmt, struct tm *tm);
void str_catftime(STRUCT_STR *sa, const char *fmt, struct tm *tm);

#ifdef __cplusplus
}; /* end of function prototypes */
#endif

#endif
