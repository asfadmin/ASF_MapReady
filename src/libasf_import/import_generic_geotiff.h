#ifndef _IMPORT_GENERIC_GEOTIFF_H_
#define _IMPORT_GENERIC_GEOTIFF_

#ifndef BOOL
# define BOOL char
#endif
#define MAX_FILENAME_LEN 256

#define MISSING_ASCII_DATA      "???"
#define DHFA_UNKNOWN_PROJECTION (-1)

/* Defines for output to stdio etc */
#define TABSTRING "                                                                                          "
#define TABSTRING_LEN 80
#define TAB_LEN 2

void usage (const char *name);
unsigned char local_machine_is_little_endian();

#endif // _IMPORT_GENERIC_GEOTIFF_H_
