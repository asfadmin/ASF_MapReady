#include "asf.h"
#include "asf_meta.h"
#include "asf_raster.h"

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
  int ii, kk;
  
  if (strncmp(bands, channel, 2) == 0)
    return 0;
  for (ii=1; ii<band_count; ii++) {
    for (kk=0; kk<3; kk++)
      bands++;
    if (strncmp(bands, channel, 2) == 0)
      return ii;
  }
  return -1;
}
