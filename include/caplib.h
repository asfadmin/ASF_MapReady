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
void FREE(void *ptr);
FILE *FOPEN(const char *file,const char *mode);
size_t FREAD(void *ptr,size_t size,size_t nitems,FILE *stream);
size_t FWRITE(const void *ptr,size_t size,size_t nitems,FILE *stream);
int FSEEK(FILE *stream,int offset,int ptrname);
int FSEEK64(FILE *stream,long long offset, int ptrname);
long long FTELL64(FILE *stream);
int FCLOSE(FILE *stream);
int FFLUSH(FILE *stream);
char *STRDUP(const char *string);

void programmer_error(char *mess);

#endif
