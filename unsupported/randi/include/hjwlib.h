/* hjwlib.h
* header for hjwlib.c
*/

/* increment of memory allocation useing fgets_unknown_length(); */
#define ALLOCINC	50

/* global variables */
extern char *g_hjw_filename;

/* prototypes */
FILE *fopendef();
char *fgets_unknown_length();
char **edit_lines();
