/******************************************************************************
NAME: geotiffdump - To extract TIFF and GeoTIFF tags to stdout

SYNOPSIS:  geotiffdump <inTIFF>

DESCRIPTION:
        Extracts TIFF and GeoTIFF tags from a TIFF image file and
        prints them in tabular form to stdout.

NOTE:
        A uint8 (below) is a libtiff BYTE, a uint32 is a libtiff LONG,
        and a uint32 is a libtiff LONG

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    0.0     9/06   B. Dixon     Original code

******************************************************************************/
#include <asf.h>
#include <geotiff.h>
#include <geotiffio.h>
#include <tiff.h>
#include <tiffio.h>
#include <xtiffio.h>
#include "geotiffdump.h"

/***** main() *****/
int main (int argc, char *argv[])
{
  char dump_metadata; /* Not implemented */
  char suppress_TIFF_hdr;
  char force_hex_output; /* Not implemented */
  char IFDdumpoffset; /* Not implemented */
  char helpme;
  char infile[256];
  char errmsg[256];
  TIFFHeader tiff_hdr;
  uint8 magic[2];
  uint16 endian;
  uint16 curIFDnum;
  uint16 flags;
  FILE *fp;
  TIFF *tiff;
  GTIF *gtif;

  /***** Process the command line arguments ******/
  /* If help option or no arguments, print usage */
  GTDparse_command_line(argc, argv, infile, &flags, &dump_metadata,
    &suppress_TIFF_hdr, &force_hex_output, &IFDdumpoffset, &helpme);
  if (helpme || argc < 2) {
    GTDusage(argv[0]);
    exit(EXIT_FAILURE);
  }

  /***** Open/Close TIFF file for reading TIFF file header info *****/
  /*                                                                */
  fp = fopen(infile, "r");
  asfRequire(fp != NULL, "\nERROR: Error opening input TIFF file.\n\n");
  fread(&tiff_hdr, sizeof(TIFFHeader), 1, fp);
  fclose(fp);

  endian = (tiff_hdr.tiff_magic == TIFF_BIGENDIAN) ? TIFF_BIGENDIAN :
	   (tiff_hdr.tiff_magic == TIFF_LITTLEENDIAN) ? TIFF_LITTLEENDIAN :
           INVALID_ENDIAN;
  asfRequire(endian == TIFF_BIGENDIAN || endian == TIFF_LITTLEENDIAN,
    "\nERROR: Error reading endian values from input TIFF file or\n"
    "invalid input TIFF file\n\n");

  if (endian == TIFF_BIGENDIAN &&
      local_machine_is_little_endian()) {
    TIFFSwabShort(&tiff_hdr.tiff_magic);
    TIFFSwabShort(&tiff_hdr.tiff_version);
    TIFFSwabLong(&tiff_hdr.tiff_diroff);
  }

  /***** Open Tiff file for IFD, TIFF Tag, and GeoTIFF Tag info *****/
  /*                                                                */
  tiff = XTIFFOpen(infile, "r"); /* TIFF header and first IFD are read
                                    by default upon opening */
  asfRequire(tiff != NULL, "\nERROR: Error opening input TIFF file.\n\n");

  gtif = GTIFNew(tiff); /* Note:  File must already be opened via XTIFFOpen()
                           or TIFFOpen() before calling GTIFNew() */
  asfRequire(gtif != NULL,
    "\nERROR: Error reading GeoTIFF keys from input TIFF file.\n\n");

  // Grab the citation.
//  size_t max_citation_length = 10000;
//  char *citation = MALLOC ((max_citation_length + 1) * sizeof (char));
//  GTIFKeyGet (gtif, GTCitationGeoKey, citation, 0, max_citation_length);
  // Ensure the citation is at least eventually terminated somewhere.
//  citation[max_citation_length] = '\0';
//  printf("\n\n--------------Citation --------------\n%s\n--------------Citation--------------\n\n", citation);


  /***** Dump TIFF File Header Info *****/
  /*                                    */
  curIFDnum = 0;
  if (!suppress_TIFF_hdr) {
    printf("\nTIFF File Header at offset 0x00000000\n"
           "TIFF file: %s\n"
           "Byte order: 0x%04x ('%c%c') <%s>\n"
            "Version: 0x%04x (%d)\n"
           "Directory %d offset: 0x%08x (%d)\n\n",
           argv[argc-1],
           tiff_hdr.tiff_magic,
           ((uint8*)&tiff_hdr.tiff_magic)[0],
           ((uint8*)&tiff_hdr.tiff_magic)[1],
           (endian == TIFF_BIGENDIAN) ? "big-endian" :
  	     (endian == TIFF_LITTLEENDIAN) ? "little-endian" :
             "invalid",
           tiff_hdr.tiff_version, tiff_hdr.tiff_version,
           curIFDnum, tiff_hdr.tiff_diroff, tiff_hdr.tiff_diroff);
  }

  /***** Get first IFD (a.k.a. 'Directory') *****//*
  /*                                            */
  TIFFPrintDirectory(tiff, stdout, flags);
  printf("\n");

  /**** Cleanup *****/
  /*                */
  GTIFFree(gtif);
  XTIFFClose(tiff);

  return(EXIT_SUCCESS);
}

/***** Usage *****/
void GTDusage (const char *name)
{
  asfRequire(name != NULL,
    "\nERROR: GTDusage(): NULL name pointer encountered.\n\n");

  printf("\n"
	 "USAGE\n"
	 "   %s [-AScmqads?] <inTIFF>\n",name);
  printf("\n"
	 "REQUIRED ARGUMENTS\n"
	 "   <inTIFF>      Base name for (input) TIFF or GeoTIFF image"
         " file.\n");
  printf("\n"
         "OPTIONS\n"
         "   Command line options are described below.\n\n"
         "   -A  Print with all options (default).\n\n"
         "   -S  Print strip/tile info.\n\n"
         "   -c  Print color/gray response curves.\n\n"
         "   -m  Print colormap.\n\n"
         "   -q  Printle JPEG Q matrices.\n\n"
         "   -a  Print JPEG AC tables.\n\n"
         "   -d  Print JPEG DC tables.\n\n"
         "   -s  Suppress printing of the TIFF file header.\n\n"
         "   -?  Print this help information\n");
  printf("\n"
	 "DESCRIPTION\n"
	 "   Extracts TIFF file header, first (only) image format directory"
         " (IFD), GeoTIFF directory, and GeoTIFF tags to stdout.\n\n"
         "   Note that if an option-request TAG or DATA is not present"
         " in the TIFF file, that the requested output will not\n"
         "   occur.  The program will automatically determine the TIFF"
         " file and local host's endianness and will display the\n"
         "   information appropriately.\n");
  printf("\n"
	 "Version %.1f\n"
	 "\n", VERSION);
}

/******************** local_machine_is_little_endian() ********************/
/*  COMMENTS:                                                             */
/*    - Returns non-zero if the local machine architecture/OS writes      */
/*      data to memory/disk in little-endian order, e.g. Intel format     */
/*      else returns zero                                                 */
/*    - 'testlong' is a multi-byte unsigned integer with a non-zero value */
/*      stored in its least-significant byte (LSB).  If the local machine */
/*      writes in big-endian format, then '*(uint8*)&testlong' (the byte  */
/*      at the lowest address in memory where 'testlong' is written) will */
/*      be zero.  But if the local machine is writing in little-endian    */
/*      format, then the bytes of 'testlong' will be in reverse order     */
/*      and the LSB will be located at the lowest byte address, e.g.      */
/*      '*(uint8*)&testlong' will return the LSB ...non-zero in this case */
uint8 local_machine_is_little_endian()
{
  uint32 testlong=1L;
  uint8 rtn_val = *(uint8*)&testlong;

  return rtn_val;
}

void GTDparse_command_line(int argc, char *argv[], char* infile,
  uint16* flags, char *dump_metadata, char *suppress_TIFF_hdr,
  char *force_hex_output, char *IFDdumpoffset, char *helpme)
{
  char option = getopt(argc, argv, GTD_OPTION_LIST);

  if (argc > 1) {
    strcpy(infile, argv[argc-1]);
  }
  *flags = 0;
  *dump_metadata = 0;
  *suppress_TIFF_hdr = 0;
  *force_hex_output = 0;
  *IFDdumpoffset = 0;
  *helpme = 0;

  /* If only the filename is given, default to all options selected */
  /* other than suppression of the header output.                   */
  if (argc == 2) {
    option = 'A';
  }

  /* Parse out all command line options */
  while (option >= 0) { /* getopt() returns -1 for failure or end of list */
    switch (option) {
      case 'A':
        *flags |= TIFFPRINT_STRIPS |
                  TIFFPRINT_CURVES |
                  TIFFPRINT_COLORMAP |
                  TIFFPRINT_JPEGQTABLES |
                  TIFFPRINT_JPEGACTABLES |
                  TIFFPRINT_JPEGDCTABLES;
        *dump_metadata = 0; /* NOT implemented */
        *suppress_TIFF_hdr = 0;
        *force_hex_output = 0; /* NOT implemented */
        *IFDdumpoffset = 0; /* NOT implemented */
        break;
      case 'S':
        *flags |= TIFFPRINT_STRIPS;
        break;
      case 'c':
        *flags |= TIFFPRINT_CURVES;
        break;
      case 'm':
        *flags |= TIFFPRINT_COLORMAP;
        break;
      case 'q':
        *flags |= TIFFPRINT_JPEGQTABLES;
        break;
      case 'a':
        *flags |= TIFFPRINT_JPEGACTABLES;
        break;
      case 'd':
        *flags |= TIFFPRINT_JPEGDCTABLES;
        break;
      case 't':
        *dump_metadata = 0; /* NOT implemented */
        break;
      case 's':
        *suppress_TIFF_hdr = 1;
        break;
      case 'h':
        *force_hex_output = 0; /* NOT implemented */
        break;
      case 'o':
        *IFDdumpoffset = 0; /* NOT implemented */
        break;
      case '?':
        *helpme = 1;
        break;
      default:
        break;
    }
    option = getopt(argc, argv, GTD_OPTION_LIST);
  } /* end while(option > 0) */
}

