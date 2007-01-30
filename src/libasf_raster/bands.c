#include "asf.h"
#include "asf_meta.h"
#include "asf_raster.h"
#include <ctype.h>

// Assumes comma-delimited 2-digit or 2-character band names
// with no white space (before, in, or after the band info)
// Ex) If 4 bands exist: "01,02,03,04" or "HH,HV,VH,VV" etc
//    => Result bands[0] == "01", bands[1] == "02" etc
char **extract_band_names(char *bands, int band_count)
{
  char **band_name;
  int ii, kk;

  band_name = (char **) MALLOC(band_count*sizeof(char *));
  for (ii=0; ii<band_count; ii++) {
    band_name[ii] = (char *) MALLOC(10*sizeof(char));
    strncpy(band_name[ii], bands, 2);
    band_name[ii][2] = '\0';
    if (ii < band_count-1)
      for (kk=0; kk<3; kk++)
        bands++;
  }
  return band_name;
}

/*
 Sample usage:
   const char *rgb = "HH,VH,VV"; // probably would be read from a .meta file
   char *r, *b, *g;
   if (split3(rgb, &r, &g, &b, ',')) {
      // Success! Now: r=="HH" g=="VH" and b=="VV"
      FREE(r); FREE(g); FREE(b);
   } else
      // failed -- don't call FREE.
*/
int split3(const char *rgb, char **pr, char **pg, char **pb, char sep)
{
    // call them commas, even though could be something else
    char *comma1 = strchr(rgb, sep);
    if (!comma1) return FALSE;

    char *comma2 = strchr(comma1+1, sep);
    if (!comma2) return FALSE;

    // allocating too much space...
    char *r = MALLOC(sizeof(char)*(strlen(rgb)+1));
    char *g = MALLOC(sizeof(char)*(strlen(rgb)+1));
    char *b = MALLOC(sizeof(char)*(strlen(rgb)+1));
    *pr = r; *pg = g; *pb = b;

    strcpy(r, rgb);
    r[comma1-rgb]='\0';

    strcpy(g, comma1+1);
    g[comma2-comma1-1]='\0';

    strcpy(b, comma2+1);
    return TRUE;
}

// Returns an array of strings which represent band numbers,
//
char **find_bands(char *in_base_name, char *red_channel, char *green_channel,
		  char *blue_channel)
{
  char **rgb=NULL;
  meta_parameters *meta;
  int ii, red=0, green=0, blue=0;

  meta = meta_read(in_base_name);
  // Check for bands
  if (strcmp(meta->general->bands, "???") != 0) {
    if (strstr(meta->general->bands, red_channel)) red = 1;
    if (strstr(meta->general->bands, green_channel)) green = 1;
    if (strstr(meta->general->bands, blue_channel)) blue = 1;
  }

  // Found three channels for RGB?
  if (red && green && green) {
    rgb = (char **) MALLOC(3*sizeof(char *));
    for (ii=0; ii<3; ii++) {
      rgb[ii] = (char *) MALLOC(10*sizeof(char));
      //strncpy(rgb[ii], meta->general->bands, 2);
      //rgb[ii][2] = '\0';
    }
    strcpy(rgb[0], red_channel);
    strcpy(rgb[1], green_channel);
    strcpy(rgb[2], blue_channel);
  }

  return rgb;
}

// Given a string that represents a band number, e.g. "03" or "3"
// or "HH" or "VH" etc and a list of bands in CSV format, e.g.
// "01,02,03" or "HH,HV,VH,VV" etc, get_band_number() returns
// the position within the list of bands where the band string
// was found (0-ordered).
//
// Ex) "VH" should result in a return of 2 in the bands
// string "HH,HV,VH,VV"
//
// Valid channel numbers are 1 thru MAX_BANDS, with or
//   without leading zeros.
// Valid channel alpha strings are "HH", "HV", "VH", and
//   "VV", lower or upper case.
//
int get_band_number(char *bands, int band_count, char *channel)
{
  char *t_bands;
  char *t_channel;
  char *s;
  char *ptrptr;
  char **band;
  int i;
  int found;
  int band_no;

  /////////////////////////////////////////////////////////////////////
  // Get everything ready for finding the channel in the list of
  // bands (try to bullet-proof things a bit since metadata is an
  // editable text file)
  //
  // Copy and trim leading/trailing white space
  t_bands = (char*) MALLOC(sizeof(char) * strlen(bands));
  t_channel = (char*) MALLOC(sizeof(char) * strlen(bands));
  strcpy(t_bands, bands);
  s = channel;
  while (isspace(*s) && *s != '\0') s++;
  strcpy(t_channel, s);
  for (i = strlen(t_channel) - 1; i >= 0; i--) {
    if (isspace(t_channel[i])) {
      t_channel[i] = '\0';
    }
    else {
      break;
    }
  }

  // Tokenize
  band = (char **) MALLOC(sizeof(char*) * (band_count+1));
  band[0] = strtok_r(t_bands, ",", &ptrptr);
  for (i = 0; band[i] != NULL;) {
    i++;
    band[i] = strtok_r(NULL, ",", &ptrptr);
  }

  /////////////////////////////////////////////////////////////////////
  // Find band in list of bands
  //
  // => Returns first find when searching from left to
  // right in list of bands.  No error on duplicates.
  // Alpha and numeric channels are never negative
  //
  int iChannel = atoi(t_channel);
  asfRequire(iChannel >= 0,
             "Invalid channel\n");
  if (iChannel > 0) {
    // Channel is numeric
    asfRequire(iChannel > 0 && iChannel <= MAX_BANDS,
               "Invalid channel number\n");
    found = 0;
    for (band_no = 0; !found && band_no < band_count; band_no++) {
      int iBand_no = atoi(band[band_no]);
      asfRequire(iBand_no > 0 && iBand_no <= MAX_BANDS,
                 "Invalid channel number in list of bands\n");
      if (iChannel == iBand_no) {
        found = 1;
      }
    }
    band_no--;
  }
  else {
    // Channel is alpha
    found = 0;
    for (band_no = 0; !found && band_no < band_count; band_no++) {
      if (strstr(band[band_no], t_channel) != NULL) {
        found = 1;
      }
    }
    band_no--;
  }

  FREE(t_bands);
  FREE(t_channel);

  if (found)
    return band_no;
  else
    return -1;
}
