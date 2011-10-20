#include "asf.h"
#include "asf_meta.h"
#include "asf_raster.h"
#include <assert.h>

static const int MAX_DN = MAX_LUT_DN;

static void lut_interp(unsigned char *buf, int left, int right)
{
  int ii;
  float r,g,b;

  r = (float)(buf[right*3] - buf[left*3]) / (float)(right-left);
  g = (float)(buf[right*3+1] - buf[left*3+1]) / (float)(right-left);
  b = (float)(buf[right*3+2] - buf[left*3+2]) / (float)(right-left);

  for (ii=left+1; ii<right; ++ii) {
      assert(ii*3+2 < MAX_LUT_DN*3);
      buf[ii*3] = (unsigned char)((r * (ii-left) + 0.5) + buf[left*3]);
      buf[ii*3+1] = (unsigned char)((g * (ii-left) + 0.5) + buf[left*3+1]);
      buf[ii*3+2] = (unsigned char)((b * (ii-left) + 0.5) + buf[left*3+2]);
  }
}

int is_jasc_palette_lut(const char *name)
{
  FILE *fp = NULL;
  char header[1024]     = "";
  char s_version[1024]  = "";
  char lut_path[1024] = "";
  int version = 0;
  int ret = 0;

  // First just check to see if the file exists, returning false if not..
  if (!fileExists(name)) {
    // Name was not found ...maybe it's in the share dir
    sprintf(lut_path, "%s%clook_up_tables%c%s",
            get_asf_share_dir(),
            DIR_SEPARATOR,
            DIR_SEPARATOR,
            name);
  }
  else {
    strcpy(lut_path, name);
  }

  // Read the file header and find out if it is a JASC palette file
  fp = (FILE *) fopen (lut_path, "r");
  if (fp) {
    fgets(header, 1024, fp);
    if (!feof(fp)) fgets(s_version, 1024, fp);
    version = atoi(s_version);
    ret = (strncmp(header, "JASC", 4) == 0 && version == 100) ? 1 : 0;
  }
  else {
    ret = 0;
  }
  FCLOSE(fp);

  return ret;
}

int read_lut(char *lutFile, unsigned char *lut_buffer)
{
  int max_dn = 0;
  FILE *fp;
  char heading[1024];
  int n,ii;
  int is_jasc_lut = is_jasc_palette_lut(lutFile);

  // fill with zeroes initially
  for (ii=0; ii<MAX_DN*3; ii++)
      lut_buffer[ii] = 0;

  fp = fopen(lutFile, "r");
  if (!fp) {
      // look in the share dir
      sprintf(heading, "%s%clook_up_tables%c%s",
              get_asf_share_dir(), DIR_SEPARATOR,
              DIR_SEPARATOR, lutFile);
      fp = fopen(heading, "r");
      if (!fp) {
          // try adding an extension...
          sprintf(heading, "%s.lut", lutFile);
          if (!fileExists(heading)) {
            sprintf(heading, "%s.pal", lutFile);
          }
          fp = fopen(heading, "r");
          is_jasc_lut = is_jasc_palette_lut(heading);
          if (!fp) {
              // try looking in the share dir, with the extension
              sprintf(heading, "%s%clook_up_tables%c%s%s",
                      get_asf_share_dir(), DIR_SEPARATOR, DIR_SEPARATOR, lutFile,
                      ".lut");
              if (!fileExists(heading)) {
                sprintf(heading, "%s%clook_up_tables%c%s%s",
                        get_asf_share_dir(), DIR_SEPARATOR, DIR_SEPARATOR, lutFile,
                        ".pal");
              }
              fp = fopen(heading, "r");
              is_jasc_lut = is_jasc_palette_lut(heading);
              if (!fp) {
                  // ok, now give up.
                  asfPrintError("Couldn't open look up table file: %s\n",
                                lutFile);
              }
          }
      }
  }

  // l: line number
  // vl: valid line number (counts lines with actual data)
  int l=0, vl=0, dn, dn_prev=0, style = is_jasc_lut ? 2 : 0;
  int num_elements = 256; // Default maximum for byte data
  char *p;

  if (is_jasc_lut) {
    // Read version number then number of elements ...leaving the file pointer pointing
    // at the first row of look-up table values (3 element type, style == 2);
    char magic_str[1024];
    char version_s[1024];
    char num_elements_s[1024];
    p = fgets(magic_str, 1024, fp);
    if (!p) asfPrintError("JASC type look-up table file - no header?\n"); // eof
    p = fgets(version_s, 1024, fp);
    if (!p) asfPrintError("JASC type look-up table file - no version?\n"); // eof
    p = fgets(num_elements_s, 1024, fp);
    if (!p) asfPrintError("JASC type look-up table file - no number of elements string?\n"); // eof
    int version = atoi(version_s);
    num_elements = atoi(num_elements_s);
    if (strncmp(magic_str, "JASC", 4) != 0) {
      asfPrintError("Bad JASC look-up table header (%s)\n", magic_str);
    }
    if (version != 100) {
      asfPrintError("Bad JASC look-up table version (%d)\n", version);
    }
    if (num_elements <= 0 || num_elements > 512) {
      asfPrintError("Bad number of elements (%d) in JASC look-up table file.\n", num_elements);
    }
    style = 2;
  }

  for (ii=0; ii<MAX_DN*3 && ii<num_elements*3; ii+=3) {
    int red, green, blue;

    p = fgets(heading, 1024, fp);
    if (!p) break; // eof
    ++l; // increment line number

    while (heading[0] == '#') {
        // read more lines if that one was a comment
        p = fgets(heading, 1024, fp);
        if (!p) break; // eof
        ++l;
    }

    // style==1 --> 4 values per line: dn red green blue
    // style==2 --> 3 values per line: red green blue
    // we don't allow mixing of the styles, first line checks

    if (!style)
    {
        // on the non-comment line, figure out which style of lutFile this is.
        n = sscanf(heading, "%d %d %d %d",
                   &dn, &red, &green, &blue);
        if (n != 4) {
            n = sscanf(heading, "%d %d %d", &red, &green, &blue);
            if (n != 3) {
                asfPrintError("Line %d of lutFile appears invalid:\n%s\n",
                              l, heading);
            }
            dn = 0;
            style = 2;
        } else
            style = 1;

    }
    else
    {
        // subsequent lines must follow first valid line's style
        if (style == 1) {
            n = sscanf(heading, "%d %d %d %d",
                       &dn, &red, &green, &blue);
            if (n != 4) break;
        } else if (style == 2) {
            n = sscanf(heading, "%d %d %d", &red, &green, &blue);
            if (n != 3) break;
            dn = vl;
        } else
            asfPrintError("read_lut> Confused: lut style==%d\n", style);
    }
    max_dn = (dn > max_dn)? dn : max_dn;

    ++vl; // increment valid data line count

    if (dn<0 || dn>MAX_DN-1 || red<0 || red>255 || green<0 || green>255 ||
        blue<0 || blue>255)
    {
        asfPrintError("read_lut> Illegal values on line %d in %s: "
                      "%d %d,%d,%d\n",
                      l, lutFile, dn, red, green, blue);
    }

    if (dn_prev>dn)
    {
        asfPrintError("read_lut> In look up table file: %s\n"
                      "On line %d, index values are not in order: %d -> %d\n",
                      lutFile, l, dn_prev, dn);
    }

    assert(dn*3+2 < MAX_LUT_DN*3);
    lut_buffer[dn*3] = (unsigned char)red;
    lut_buffer[dn*3+1] = (unsigned char)green;
    lut_buffer[dn*3+2] = (unsigned char)blue;

    if (dn-dn_prev > 1)
        lut_interp(lut_buffer,dn_prev,dn);

    dn_prev=dn;
  }
  FCLOSE(fp);

  return max_dn;
}

void apply_look_up_table_byte(char *lutFile, unsigned char *in_buffer,
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
      lut_buffer = (unsigned char *) MALLOC(sizeof(unsigned char) * MAX_DN*3);
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
}

void apply_look_up_table_int(char *lutFile, int *in_buffer,
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
      lut_buffer = (unsigned char *) MALLOC(sizeof(unsigned char) * MAX_DN*3);
      read_lut(lutFile, lut_buffer);

      // save this filename for future calls
      lut_filename = STRDUP(lutFile);
  }

  // Apply the look up table
  for (ii=0; ii<pixel_count; ii++) {
    int v = in_buffer[ii];
    if (v>MAX_DN-1) v=MAX_DN-1;
    if (v<0) v=0;
    rgb_buffer[ii*3] = lut_buffer[v*3];
    rgb_buffer[(ii*3)+1] = lut_buffer[v*3+1];
    rgb_buffer[(ii*3)+2] = lut_buffer[v*3+2];
  }
}
