#include "asf.h"
#include "asf_meta.h"
#include "asf_raster.h"

void read_lut(char *lutFile, unsigned char *lut_buffer)
{
  FILE *fp;
  char heading[1024];
  int ii, red, green, blue;

  fp = FOPEN(lutFile, "r");
  fgets(heading, 1024, fp);
  for (ii=0; ii<768; ii+=3) {
    fscanf(fp, "%d,%d,%d", &red, &green, &blue);
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
