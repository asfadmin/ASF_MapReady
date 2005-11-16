
#ifndef _ASF_GRIDFLOAT_H_
#define _ASF_GRIDFLOAT_H_

// Mirror of the .hdr file that comes with the USGS Seamless server's gridFloat
// downloadable fileset
typedef struct
{
  int ncols;            // width of image
  int nrows;            // height of image
  double xllcorner;     // lower-left corner lat
  double yllcorner;     // lower left corner lon
  double cellsize;      // pixel size (degrees)
  int NODATA_value;     // value indicating pixel is invalid
  char byteorder[64];   // little-endian vs. big-endian
} gridFloat_hdr_t;


// Mirror of the .prj file that comes with the USGS Seamless server's gridFloat
// downloadable fileset
typedef struct {
  char projection[64];
  char datum[64];
  char spheroid[64];    //
  char units[64];       // units in the horizontal (xy) plane
  char zunits[64];      // units in the vertical (z) direction
} gridFloat_prj_t;

void
read_gridFloat_hdr( char *gf_hdr_name, gridFloat_hdr_t *gf_hdr);

void
read_gridFloat_prj( char *gf_prj_name, gridFloat_prj_t *gf_prj);

#endif // _ASF_GRIDFLOAT_H_
