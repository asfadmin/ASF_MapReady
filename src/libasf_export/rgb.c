#include "asf.h"

// Detect whether a band is one of the RGB channels
static void
detect_channel(char **in_base_names, char *in_base_name,
               char *red_channel, char *green_channel, char *blue_channel,
               char *band, int *red, int *green, int *blue)
{
  *red = 0;
  *green = 0;
  *blue = 0;
  if (strcmp(band, red_channel) == 0) {
    *red = 1;
    in_base_names[0] = (char *) MALLOC(512*sizeof(char));
    sprintf(in_base_names[0], "%s_%s", in_base_name, band);
  }
  else if (band[1] == red_channel[0] && atoi(band)) {
    *red = 1;
    in_base_names[0] = (char *) MALLOC(512*sizeof(char));
    sprintf(in_base_names[0], "%s_0%c", in_base_name, band[1]);
  }
  if (strcmp(band, green_channel) == 0) {
    *green = 2;
    in_base_names[1] = (char *) MALLOC(512*sizeof(char));
    sprintf(in_base_names[1], "%s_%s", in_base_name, band);
  }
  else if (band[1] == green_channel[0] && atoi(band)) {
    *green = 2;
    in_base_names[1] = (char *) MALLOC(512*sizeof(char));
    sprintf(in_base_names[1], "%s_0%c", in_base_name, band[1]);
  }
  if (strcmp(band, blue_channel) == 0) {
    *blue = 4;
    in_base_names[2] = (char *) MALLOC(512*sizeof(char));
    sprintf(in_base_names[2], "%s_%s", in_base_name, band);
  }
  else if (band[1] == blue_channel[0] && atoi(band)) {
    *blue = 4;
    in_base_names[2] = (char *) MALLOC(512*sizeof(char));
    sprintf(in_base_names[2], "%s_0%c", in_base_name, band[1]);
  }
}

int find_bands(char ***p_in_base_names, char *in_base_name,
               char *red_channel, char *green_channel, char *blue_channel)
{
  int ii, rgb=0, red=0, green=0, blue=0, check_sum=0;
  char **in_base_names;
  char tmp[512], band[512];

  // Read in bands
  in_base_names = (char **) MALLOC(MAX_BANDS*sizeof(char *));
  for (ii=0; ii<MAX_BANDS; ii++)
    in_base_names[ii] = NULL;
  sprintf(band, "HH");
  sprintf(tmp, "%s_%s.img", in_base_name, band);
  if (fileExists(tmp)) {
    detect_channel(in_base_names, in_base_name,
                   red_channel, green_channel, blue_channel, band, 
		   &red, &green, &blue);
    check_sum += red + green + blue;
  }
  sprintf(band, "HV");
  sprintf(tmp, "%s_%s.img", in_base_name, band);
  if (fileExists(tmp)) {
    detect_channel(in_base_names, in_base_name,
                   red_channel, green_channel, blue_channel, band, 
		   &red, &green, &blue);
    check_sum += red + green + blue;
  }
  sprintf(band, "VH");
  sprintf(tmp, "%s_%s.img", in_base_name, band);
  if (fileExists(tmp)) {
    detect_channel(in_base_names, in_base_name,
                   red_channel, green_channel, blue_channel, band, 
		   &red, &green, &blue);
    check_sum += red + green + blue;
  }
  sprintf(band, "VV");
  sprintf(tmp, "%s_%s.img", in_base_name, band);
  if (fileExists(tmp)) {
    detect_channel(in_base_names, in_base_name, 
                   red_channel, green_channel, blue_channel, band, 
		   &red, &green, &blue);
    check_sum += red + green + blue;
  }
  if (check_sum == 0) {
    for (ii=1; ii<10; ii++) {
      sprintf(band, "0%d", ii);
      sprintf(tmp, "%s_%s.img", in_base_name, band);
      if (fileExists(tmp)) {
	detect_channel(in_base_names, in_base_name,
                       red_channel, green_channel, blue_channel, band, 
		       &red, &green, &blue);
	check_sum += red + green + blue;
      }
    }
  }
  if (check_sum == 0) {
    sprintf(tmp, "%s.img", in_base_name);
    in_base_names[ii] = (char *) MALLOC(512*sizeof(char));
    strcpy(in_base_names[0], in_base_name);
  }

  // Found three channels for RGB?
  if (check_sum == 7)
    rgb = 1;

  *p_in_base_names = in_base_names;
  return rgb;
}
