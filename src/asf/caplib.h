#ifndef _CAPLIB_H_
#define _CAPLIB_H_

#include <errno.h>
#include <stdarg.h>
#include <stdio.h>

#ifndef win32
#include <unistd.h>
#endif

#include "asf.h"

typedef enum {
    BEHAVIOR_ON_ERROR_ABORT,
    BEHAVIOR_ON_ERROR_CONTINUE
} behavior_on_error_t;

extern behavior_on_error_t caplib_behavior_on_error;

void *MALLOC(size_t size);
void *CALLOC(size_t nmemb, size_t size);
void FREE(void *ptr);
void FREE_BANDS(char **ptr);
FILE *FOPEN(const char *file,const char *mode);
size_t ASF_FREAD(void *ptr,size_t size,size_t nitems,FILE *stream);
size_t FREADZ(void *ptr,size_t size,size_t nitems,FILE *stream);
size_t FREAD_CHECKED(void *ptr, size_t size, size_t nitems, FILE *stream, int short_ok);
size_t ASF_FWRITE(const void *ptr,size_t size,size_t nitems,FILE *stream);
int FSEEK(FILE *stream,int offset,int ptrname);
int FSEEK64(FILE *stream,long long offset, int ptrname);
long long FTELL64(FILE *stream);
int FCLOSE(FILE *stream);
int FFLUSH(FILE *stream);
char *STRDUP(const char *string);
char *STRDUP_PLUS(const char *string, int addl_chars);

void programmer_error(char *mess);

#endif
