#ifndef _display_h__
#define _display_h__
/*-------------------------------------------
* FILE:
*   display.h
*
*------------------------------------------*/

static char sccsid_display_h[] = "@(#)display.h	1.2 96/05/17 09:24:49";


#define SMALL_FONT "-*-*-demi-r-*-*-12-*"

#define STATE_TABLE_NAME_MAX_CHARS 8  /* length of string where we resize the font */
#define STATE_TABLE_NAME_MAX_PIXELS 120 /* pixel length of name toggle button */

#define STATE_FORMAT "%s-%d"  /* this string used for state color display */

static char *states[]={"name","not running", "waiting", "ready", 
                       "running", "qc", "hold ", "error"};

static char *colors[]={"yellow", "dodgerblue", "violet", "yellow", 
                       "green", "orange", "maroon", "red"};
static char *def_colors[]={"black", "lightslategray", "lightslategray",
      "lightslategray",  "lightslategray", "lightslategray",
      "lightslategray", "lightslategray"};

static char *labels[]={"rds", "ssp1", "ssp2-a", "ssp2-b", "ssp2-c", 
                       "ssp2-d", "ssp2-e", "asp"};



#endif       /* _display_h__ */

