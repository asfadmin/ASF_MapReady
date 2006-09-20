/****************************************************************
NAME:  geotiffdump

PROGRAM HISTORY:  
 VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    0.0	 9/06   B. Dixon     Extract TIFF and GeoTIFF tags to stdout

****************************************************************/
#ifndef __geotiffdump_h
#define __geotiffdump_h

#define INVALID_ENDIAN -1
#define VERSION 0.0

/***** Option List *****/
/* To add single-character options, "-h" for example, for geotiffdump, */
/* add them to the following list, but ALSO handle them in the         */
/* switch statement in GTDparse_command_line() (see geotiffdump.c)     */
#define GTD_OPTION_LIST "AScmqadtsho?"

/***** Prototypes *****/
void GTDusage (const char *name);
uint8 local_machine_is_little_endian(void);
void GTDparse_command_line(int argc, char *argv[], char* infile,
                           uint16* flags, char *dump_metadata,
                           char *suppress_TIFF_hdr, char *force_hex_output,
                           char *IFDdumpoffset, char *helpme);



#endif /* __geotiffdump_h */
