#include "asf.h"
#include "asf_meta.h"
#include "asf_raster.h"

static void interp(unsigned char *buf, int left, int right)
{
  int ii;
  float r,g,b;

  r = (float)(buf[right] - buf[left]) / (float)(right-left);
  g = (float)(buf[right+1] - buf[left+1]) / (float)(right-left);
  b = (float)(buf[right+2] - buf[left+2]) / (float)(right-left);

  for (ii=left+1; ii<right; ii+=3) {
      buf[ii] = (unsigned char)((r * (ii-left) + 0.5) + buf[ii]);
      buf[ii+1] = (unsigned char)((g * (ii-left) + 0.5) + buf[ii+1]);
      buf[ii+2] = (unsigned char)((b * (ii-left) + 0.5) + buf[ii+2]);
  }
}

void read_lut(char *lutFile, unsigned char *lut_buffer)
{
  FILE *fp;
  char heading[1024];
  int l,n,ii;

  // fill with zeroes initially
  for (ii=0; ii<768; ii++)
      lut_buffer[ii] = 0;

  fp = fopen(lutFile, "r");
  if (!fp) {
      // look in the share dir
      sprintf(heading, "%s/look_up_tables/%s", get_asf_share_dir(), lutFile);
      fp = fopen(heading, "r");
      if (!fp) {
          // try adding an extension...
          sprintf(heading, "%s.lut", lutFile);
          fp = fopen(heading, "r");
          if (!fp) {
              // try looking in the share dir, with the extension
              sprintf(heading, "%s/look_up_tables/%s.lut",
                  get_asf_share_dir(), lutFile);
              fp = fopen(heading, "r");
              if (!fp) {
                  // ok, now give up.
                  asfPrintError("Couldn't open look up table file: %s\n", lutFile);
              }
          }
      }
  }

  fgets(heading, 1024, fp); // first line is a comment line... required
  l=1;
  for (ii=0; ii<768; ii+=3) {
    int dn, dn_prev=0, red, green, blue;
    fgets(heading, 1024, fp);
    ++l; // line number

    n = sscanf(heading, "%d %d,%d,%d", &dn, &red, &green, &blue);
    if (n != 4) break;
    
    if (dn<0 || dn>255 || red<0 || red>255 || green<0 || green>255 || blue<0 || blue>255) 
        asfPrintError("read_lut> Illegal values on line %d in %s: %d %d,%d,%d\n",
                      l, lutFile, dn, red, green, blue);
    if (dn_prev>dn)
        asfPrintError("read_lut> In look up table file: %s\n"
                      "On line %d, index values are not in order: %d -> %d\n",
                      lutFile, l, dn_prev, dn);

    lut_buffer[dn] = (unsigned char)red;
    lut_buffer[dn+1] = (unsigned char)green;
    lut_buffer[dn+2] = (unsigned char)blue;

    if (dn-dn_prev > 1)
        interp(lut_buffer,dn_prev,dn);

    dn_prev=dn;
  }
  FCLOSE(fp);

  // FIXME: remove this debug junk
  printf("Read: %s\n", lutFile);
  for (ii=0; ii<768; ii+=3)
      printf("%d: %d,%d,%d\n", ii/3, lut_buffer[ii], lut_buffer[ii+1], lut_buffer[ii+2]);
}

void apply_look_up_table(char *lutFile, unsigned char *in_buffer,
			 int pixel_count, unsigned char *rgb_buffer)
{
  int ii;

  // These store the most-recently-used buffer info, and the filename
  // for that buffer.  So, if this function is called a bunch of times
  // with the same lutFile, we won't read in the same file over&over again
  static unsigned char *lut_buffer=NULL;
  static char *lut_filename=NULL;

  if (!lut_filename || strcmp(lut_filename, lutFile) != 0) {
      // Free previous buffer, if any
      if (lut_buffer) FREE(lut_buffer);
      if (lut_filename) FREE(lut_filename);

      // Read look up table
      lut_buffer = (unsigned char *) MALLOC(sizeof(unsigned char) * 768);
      read_lut(lutFile, lut_buffer);

      // save this filename for future calls
      lut_filename = STRDUP(lutFile);
  }

  // Apply the look up table
  for (ii=0; ii<pixel_count; ii++) {
    rgb_buffer[ii*3] = lut_buffer[in_buffer[ii]*3];
    rgb_buffer[(ii*3)+1] = lut_buffer[in_buffer[ii]*3+1];
    rgb_buffer[(ii*3)+2] = lut_buffer[in_buffer[ii]*3+2];
  }

  // Clean up
  FREE(lut_buffer);
}
