/*******************************************************************************
NAME: geoLatLon

SYNOPSIS: geoLatLon(double eleva, char *metaName, char *out_tps)

DESCRIPTION:
    creates a 9x9 grid over the given input image, finds the latitude and
    longitude for each grid point, and writes the lat,lon and x,y values to
    its output file, which is later read by projectGeo.

ARGUMENTS:
   eleva      Set average terrain height to given meters.
   metaName   image metadata name (.meta, .ddr, or .L)
   out_tps    ASCII tie point file, used by projectGeo. This has the format:
                   <lat> <lon> <input X> <input Y>

FUNCTION HISTORY:
    VERS:   DATE:  AUTHOR:	PURPOSE:
    ---------------------------------------------------------------
    1.0    03/98   O. Lawlor    Original Development
    2.0    ?       R. Gens      Convert from program to function
    2.5    08/03   P. Denny     Obliterate DDR and use new metadata

HARDWARE/SOFTWARE LIMITATIONS: none

BUGS: none known

*******************************************************************************/

#include "asf.h"
#include "asf_meta.h"

#define REZ_X 8
#define REZ_Y 8

void geoLatLon(double eleva, char *metaName, char *out_tps)
{
   int x,y;               /* Loop counters              */
   FILE *out;             /* Output tie point file      */
   char message[256];     /* Blather to user & log file */
   meta_parameters *meta; /* Metadata file struct       */

/* Print the user specified average elevation of the output area */
   sprintf(message,"   Input average output elevation = %f\n\n",eleva);
   printf("%s",message);
   if (logflag) { printLog(message); }
   
   out = FOPEN(out_tps,"w");    /* Open ascii output file */
   meta = meta_read(metaName);  /* Read meta file into meta struct */

/* Create grid over image */
   for (y=0; y<=REZ_Y; y++) {
      for (x=0; x<=REZ_X; x++) {
         double lat,lon;           /* Output latitude and longitude */
         int cur_line,cur_samp;    /* Coordinates in current image  */
         int orig_samp,orig_line;  /* Coordinates in original image */
         
         cur_line = y * meta->general->line_count / REZ_Y;
         cur_samp = x * meta->general->sample_count / REZ_X;

         /* Figure out the coordinates in the original image */
         meta_get_original_line_sample(meta, cur_line, cur_samp,
                                       &orig_line, &orig_samp);

         /* Convert these coordinates to lat/lon */
         meta_get_latLon(meta, orig_line, orig_samp, eleva, &lat, &lon);

         /* Print out the lat & lon information to output file */
         fprintf(out,"%.12f %.12f %d %d\n", lat, lon, cur_samp, cur_line);
      }
   }

   FCLOSE(out);
}
