#include "asf.h"
#include "asf_meta.h"
#include "asf_raster.h"
#include <ctype.h>

// Prototypes
char *channel_trim (const char *channel);

char **extract_band_names(char *bands, int band_count)
{
  char *t_bands;
  char *t_channel;
  char *ptrptr;
  char **band_ary;
  int i;

  // Set up the band array
  band_ary = (char **) CALLOC(sizeof(char*), MAX_BANDS);

  // First do the obvious for single-band images.
  if (bands != NULL && strlen(bands) > 0) {
    t_bands = STRDUP(bands);
    if (band_count == 1) {
      if (strlen(bands) > 0) {
        band_ary = (char **) MALLOC(sizeof(char*));
        t_channel = strtok_r(t_bands, ",", &ptrptr);
        if (t_channel != NULL) {
          band_ary[0] = STRDUP(t_channel);
        }
      }
    }
    else if (band_count > 1) {
      // Handle multiple bands if multiple bands exist
      band_ary = (char **) MALLOC(sizeof(char*) * band_count);
      t_channel = strtok_r(t_bands, ",", &ptrptr);
      if (t_channel != NULL) {
        band_ary[0] = STRDUP(t_channel);
      }
      for (i = 1; i < band_count; i++) {
        t_channel = strtok_r(NULL, ",", &ptrptr);
        if (t_channel != NULL) {
          band_ary[i] = STRDUP(t_channel);
        }
      }
    }
    else {
      band_ary = NULL;
    }
  }
  else {
    band_ary = NULL;
  }

  return band_ary;
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
    if (rgb && strlen(rgb)) {
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
    else {
      return FALSE;
    }
}

// Returns an array of strings which represent bands.
//
char **find_bands(char *in_base_name, int rgb_flag, char *red_channel, char *green_channel,
      char *blue_channel, int *num_found)
{
  char **rgb=NULL;
  meta_parameters *meta;
  int ii;
  int ignored[3];
  //int red=0, green=0, blue=0;

  // ALOS fix... ALOS basenames can contain a '.' and the appendExt()
  // in meta_read() ends up truncating the basename at that point
  // prior to appending the ".meta" extension.  By adding the ".meta"
  // extension here, appendExt() will just remove and replace the same
  // extension... no harm, no foul.
  char *meta_name = (char *) MALLOC((6 + strlen(in_base_name)) * sizeof(char));
  strcpy(meta_name, in_base_name);
  append_ext_if_needed(meta_name, ".meta", NULL);
  meta = meta_read(meta_name);
  //meta = meta_read(in_base_name);
  // Check for bands
  ignored[0] = (strncmp("IGNORED", uc(red_channel), 6) == 0 && rgb_flag) ? 1 : 0;
  ignored[1] = (strncmp("IGNORED", uc(green_channel), 6) == 0 && rgb_flag) ? 1 : 0;
  ignored[2] = (strncmp("IGNORED", uc(blue_channel), 6) == 0 && rgb_flag) ? 1 : 0;
  *num_found = 0;
  rgb = (char **) MALLOC(MAX_BANDS*sizeof(char *));
  for (ii=0; ii<MAX_BANDS; ii++) {
    rgb[ii] = (char *) MALLOC(10*sizeof(char));
    strcpy(rgb[ii],"");
  }
  if (strcmp(meta->general->bands, "???") != 0) {
    if (!ignored[0]) {
      if (red_channel && strlen(red_channel) && strstr(meta->general->bands, red_channel)) {
        //red = 1;
        strcpy(rgb[*num_found], red_channel);
        (*num_found)++;
      }
      else if (rgb_flag) {
        asfPrintError("Channel specified for RED (\"%s\")"
                        " not found in image file.\n"
                        "Available channels are %s\n", red_channel,
                        meta->general->bands);
      }
    }
    else {
      strcpy(rgb[*num_found], "Ignored");
      (*num_found)++;
    }

    if (!ignored[1]) {
      if (green_channel && strlen(green_channel) && strstr(meta->general->bands, green_channel)) {
        //green = 1;
        strcpy(rgb[*num_found], green_channel);
        (*num_found)++;
      }
      else if (rgb_flag) {
        asfPrintError("Channel specified for GREEN (\"%s\")"
                        " not found in image file.\n"
                        "Available channels are %s\n", green_channel,
                        meta->general->bands);
      }
    }
    else {
      strcpy(rgb[*num_found], "Ignored");
      (*num_found)++;
    }

    if (!ignored[2]) {
      if (blue_channel && strlen(blue_channel) && strstr(meta->general->bands, blue_channel)) {
        //blue = 1;
        strcpy(rgb[*num_found], blue_channel);
        (*num_found)++;
      }
      else if (rgb_flag) {
        asfPrintError("Channel specified for BLUE (\"%s\")"
                        " not found in image file.\n"
                        "Available channels are %s\n", blue_channel,
                        meta->general->bands);
      }
    }
    else {
      strcpy(rgb[*num_found], "Ignored");
      (*num_found)++;
    }
  }

  return rgb;
}

// Returns a pointer to a string with the band (if found).
// Otherwise the string will be empty.
char **find_single_band(char *in_base_name, char *band, int *num_found)
{
  char **band_name=NULL;
  meta_parameters *meta;
  char *meta_name = (char *) MALLOC((6 + strlen(in_base_name)) * sizeof(char));
  strcpy(meta_name, in_base_name);
  append_ext_if_needed(meta_name, ".meta", NULL);
  meta = meta_read(meta_name);

  // Check for band
  *num_found = 0;
  if (strcmp(uc(band), "ALL") == 0) {
    band_name = extract_band_names(meta->general->bands,
           meta->general->band_count);
    *num_found = meta->general->band_count;
  }
  else {
    int ii;
    band_name = (char **) MALLOC(meta->general->band_count*sizeof(char *));
    for (ii=0; ii<meta->general->band_count; ii++)
      band_name[ii] = NULL;
    band_name[0] = (char *) MALLOC(10*sizeof(char));
    strcpy(band_name[0],"");
    if (strcmp(meta->general->bands, "???") != 0) {
      if (strlen(band) && strstr(meta->general->bands, band)) {
  strcpy(band_name[*num_found], band);
  (*num_found)++;
      }
    }
  }
  meta_free(meta);
  FREE(meta_name);
  return band_name;
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
  char *ptrptr;
  char **band;
  int i;
  int found;
  int band_no;

  // First do the obvious for single-band images.
  if (band_count == 1)
    return 0;

  /////////////////////////////////////////////////////////////////////
  // Copy then trim leading white space and zeros, and trailing white
  // space, from channel identifier
  t_bands = STRDUP(bands);
  t_channel = channel_trim(channel);

  // Tokenize and strip leading zeros/white space and trailing
  // white space at the same time
  band = (char **) MALLOC(sizeof(char*) * (band_count+1));
  band[0] = channel_trim(strtok_r(t_bands, ",", &ptrptr));
  for (i = 0; band[i] != NULL;) {
    i++;
    band[i] = channel_trim(strtok_r(NULL, ",", &ptrptr));
  }

  /////////////////////////////////////////////////////////////////////
  // Find band in list of bands
  //
  // => Returns first find when searching from left to
  // right in list of bands.  No error on duplicates.
  //
  found = 0;
  for (band_no = 0; !found && band_no < band_count; band_no++) {
    if (strcmp(band[band_no], t_channel) == 0) {
      found = 1;
    }
  }
  band_no--;

  // Clean up
  FREE(t_bands);
  FREE(t_channel);
  for (i=0; band[i] != NULL; i++) {
    FREE(band[i]);
  }

  if (found)
    return band_no;
  else
    return -1;
}

//
// Trims leading white space and zeros, then
// trims trailing white space.
//
// Returns:
//   - NULL is returned for all-spaces or NULL
//   channel strings
//   - If a channel string is all zeros (other than
//   leading white space), then a duplicate is returned
//   as-is
//   - Otherwise, a trimmed version of the channel
//   string is returned
//
// Requirements: Calling routine must free memory pointed
//   to by the return value (if not NULL)
//
char *channel_trim (const char *channel)
{
  char *s = (char *) channel;
  char *t_channel;
  int i;

  // Null channel ptr or zero-length channel
  // invalid ...return NULL
  if (channel == NULL || strlen(channel) == 0) {
    return NULL;
  }

  // Remove leading white space
  while (isspace(*s) && *s != '\0') s++;
  if (strlen(s) == 0) {
    // channel was all spaces ...invalid so return NULL
    return NULL;
  }

  // Remove leading zeros
  char *z = s;
  while (*z == '0' && *z != '\0') z++;
  if (strlen(z) == 0) {
    // channel was all zeros ...return as-is to support
    // the case where a channel name IS all zeros
    return STRDUP(channel);
  }

  // Remove trailing white space
  t_channel = STRDUP(z);
  for (i = strlen(t_channel) - 1; i >= 0; i--) {
    if (isspace(t_channel[i])) {
      t_channel[i] = '\0';
    }
    else {
      break;
    }
  }

  return t_channel;
}

