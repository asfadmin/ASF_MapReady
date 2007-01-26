#include "asf.h"
#include "asf_meta.h"
#include "asf_raster.h"
#include <ctype.h>

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

char **find_bands(char *in_base_name, char *red_channel, char *green_channel, 
		  char *blue_channel)
{
  char **rgb;
  meta_parameters *meta;
  int ii, red=0, green=0, blue=0;

  meta = meta_read(in_base_name);
  // Check for bands
  if (strstr(meta->general->bands, red_channel)) red = 1;
  if (strstr(meta->general->bands, green_channel)) green = 1;
  if (strstr(meta->general->bands, blue_channel)) blue = 1;
  
  // Found three channels for RGB?
  if (red && green && green) {
    rgb = (char **) MALLOC(3*sizeof(char *));
    for (ii=0; ii<3; ii++) {
      rgb[ii] = (char *) MALLOC(10*sizeof(char));
      strncpy(rgb[ii], meta->general->bands, 2);
      rgb[ii][2] = '\0';
    }
    strcpy(rgb[0], red_channel);
    strcpy(rgb[1], green_channel);
    strcpy(rgb[2], blue_channel);
  }
  else 
    rgb = NULL;

  return rgb;
}

int get_band_number(char *bands, int band_count, char *channel)
{
  char tmp[16];
  char *comma;
  char *t;
  int cmp;
  int ii, kk;
 
  strcpy(tmp, bands);
  t = tmp;
  while (!isdigit(*t)) t++;
  while (isdigit(*t) && *t == '0') t++;
  comma = strchr(t, ',');
  if (comma) *comma = '\0';
  if (strncmp(t, channel, 2) == 0)
    return 0;
  for (ii=1; ii<band_count; ii++) {
    for (kk=0; kk<3; kk++)
      bands++;
    strcpy(tmp, bands);
    t = tmp;
    while (!isdigit(*t)) t++;
    while (isdigit(*t) && *t == '0') t++;
    comma = strchr(t, ',');
    if (comma) *comma = '\0';
    if (strncmp(t, channel, 2) == NULL)
      return ii;
  }
  return -1;
}

