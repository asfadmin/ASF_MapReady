/* STOOUPID dummy implementation of actual, smart caplib routines. */

#define MALLOC(x) malloc(x)
#define FREE(x) free(x)
#define FOPEN(n,p) fopen(n,p)
