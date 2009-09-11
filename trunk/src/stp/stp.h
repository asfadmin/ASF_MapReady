#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <assert.h>
#include <unistd.h>
#include <ctype.h>
#include <gtk/gtk.h>
#include <glade/glade.h>
#include <glib.h>
#include <glib/gprintf.h>

/* for win32, need __declspec(dllexport) on all signal handlers */
#if !defined(SIGNAL_CALLBACK)
#  if defined(win32)
#    define SIGNAL_CALLBACK __declspec(dllexport)
#  else
#    define SIGNAL_CALLBACK
#  endif
#endif

#include "asf_version.h"

#ifdef win32
#  include <pango/pango.h>

#  define BYTE __byte
#    include "asf_meta.h"
#    include <asf_import.h>
#  undef BYTE
#  include <Windows.h>
#  include <process.h>
#  undef DIR_SEPARATOR
#else
#include "asf_meta.h"
#endif

// The thread function needs to be declared properly on Windows.
// On Linux, we are using fork() and then just call this function,
// so it need not be anything fancy.  We use the stp_params_t struct
// to hold everything that is needed, in either situation, so the
// actual code can be the same in both OSs

typedef struct {
    char input_file[1024];
    char output_file[1024];
    int status;
    float fd, fdd, fddd;
    int fd_set, fdd_set, fddd_set;
    int debug_flag;
    int ifirstline;
} stp_params_t;

static const int STATUS_OK = 1;
static const int STATUS_FILE_NOT_FOUND = 2;
static const int STATUS_META_FILE_NOT_FOUND = 3;
static const int STATUS_LDR_INSTEAD = 4;
static const int STATUS_STF_INSTEAD = 5;

static char *
change_extension(const char * file, const char * ext)
{
    char * replaced = MALLOC(sizeof(char)*(strlen(file) + strlen(ext) + 10));

    strcpy(replaced, file);
    char * p = strrchr(replaced, '.');

    if (p)
        *p = '\0';

    strcat(replaced, ".");
    strcat(replaced, ext);

    return replaced;
}
