/*  Dan Fineman's Quality Analysis app.
    Fun fun fun!
*/

#ifndef _QINCLUDE_H
#define _QINCLUDE_H

static char sccsid_qinclude_h[] =
	"@(#)ginclude.h	1.1 95/12/20 16:39:53";

/*  Widgets and gadgets... */
#include <Xm/ArrowB.h>
#include <Xm/Text.h>
#include <Xm/Separator.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/ScrolledW.h>
#include <Xm/ScrollBar.h>
#include <Xm/Label.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/List.h>
#include <Xm/SeparatoG.h>
#include <Xm/DrawingA.h>

/* for complex stuff */
#include <math.h>

/* for file-open */
#include <Xm/MessageB.h>
#include <fcntl.h> 
#include <string.h>


/* String operations */
#include <bstring.h>

/* to get the size of a file */
#include <sys/stat.h>
#include <sys/types.h>


#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>

/* for the pixmap: */
typedef unsigned char  byte;

#ifndef  IN_MAIN
#define  EXTERN_IF_NOT_IN_MAIN extern
#else
#define  EXTERN_IF_NOT_IN_MAIN
#endif

/* for cris */
#include <signal.h>

/* For the syslog */
/* #include <syslog.h> */
#include "asf_syslog.h"

#endif /* _QINCLUDE_H */
