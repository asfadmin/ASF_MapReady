#include "asf.h"
#include "asf_meta.h"
#include "asf_raster.h"

void read_lut(char *lutFile, unsigned char *lut_buffer)
{
  FILE *fp;
  char heading[1024];
  int ii, red, green, blue;

  // fill with zeros initially
  for (ii=0; ii<768; ii++)
      lut_buffer[ii] = 0;

  fp = fopen(lutFile, "r");
  if (!fp) {
      // look in the share dir
      sprintf(heading, "%s/look_up_tables/%s", get_asf_share_dir(), lutFile);
      fp = fopen(heading, "r");
      if (!fp)
          asfPrintError("Couldn't open look up table file: %s\n", lutFile);
  }

  fgets(heading, 1024, fp);
  for (ii=0; ii<768; ii+=3) {
    fgets(heading, 1024, fp);
    int n = sscanf(heading, "%d,%d,%d", &red, &green, &blue);
    if (n != 3) break;
    lut_buffer[ii] = red;
    lut_buffer[ii+1] = green;
    lut_buffer[ii+2] = blue;
  }
  FCLOSE(fp);
}

void apply_look_up_table(char *lutFile, unsigned char *in_buffer,
			 int pixel_count, unsigned char *rgb_buffer)
{
  int ii;
  unsigned char *lut_buffer;

  // Read look up table
  lut_buffer = (unsigned char *) MALLOC(sizeof(unsigned char) * 768);
  read_lut(lutFile, lut_buffer);

  // Apply the look up table
  for (ii=0; ii<pixel_count; ii++) {
    rgb_buffer[ii*3] = lut_buffer[in_buffer[ii]*3];
    rgb_buffer[(ii*3)+1] = lut_buffer[in_buffer[ii]*3+1];
    rgb_buffer[(ii*3)+2] = lut_buffer[in_buffer[ii]*3+2];
  }

  // Clean up
  FREE(lut_buffer);
}
