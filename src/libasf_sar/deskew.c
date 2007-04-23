#include "asf.h"
#include "asf_meta.h"
#include "asf_sar.h"
#include "geolocate.h"
#include "asf_raster.h"

// This is the guts of the "deskew" tool, moved here in to this
// library so we can call it from asf_terrcorr.

// Deskew uses the squint angle of an image along with the
// look angle to determine the amount of parallelogram shift
// skew that has been introduced in an image due to the doppler
// centroid choosen during image processing.  It then remaps
// the image (using bi-linear interpolation) to remove this skew.

// Giving the same input & output filename is allowed, the
// input data will be overwritten.

// This code should actually work on data that is already deskewed,
// no changes are made, output matches input

void deskew(const char *infile, const char *outfile)
{
  meta_parameters *meta = meta_read(infile);

  int nl = meta->general->line_count;
  int np = meta->general->sample_count;
  int nb = meta->general->band_count;
  char **band_name = extract_band_names(meta->general->bands, nb);

  if (!meta->sar)
    asfPrintError("Cannot deskew data without a sar block!\n");

  char *tmp_outfile;
  int do_rename = FALSE;
  if (strcmp(infile, outfile) == 0 && nb>0) {
    // user wants to deskew in-place
    // we can't actually do that on multi-band data, too much to keep in memory
    // use a temporary file, then clobber input file
    tmp_outfile = appendToBasename(outfile, "_tmp");
    do_rename = TRUE;
  } else {
    // normal case: either
    // 1) single-band in-place deskew
    // 2) not in-place deskew (single or multi)
    tmp_outfile = STRDUP(outfile);
  }

  // calculate the amount of shift necessary
  stateVector stVec = meta_get_stVec(meta, 0.0);
  GEOLOCATE_REC *g = init_geolocate_meta(&stVec, meta);

  double time, slant, dop;
  meta_get_timeSlantDop(meta, 0, 0, &time, &slant, &dop);

  double look, yaw;
  getLookYaw(g, slant, dop, &look, &yaw);
  free_geolocate(g);

  double fac = sin(look)*sin(yaw);

  int b, i, j, d=meta->sar->deskewed != 0;

  // the "lower" array stores the required shifts, indexed by column
  // (the amount of shift is row-independent)
  int *lower = MALLOC(np * sizeof(int));
  for (i=0; i<np; ++i)
    lower[i] = (int) (fac*(float)i);

  float *ibuf = MALLOC(np * sizeof(float));
  float *obuf = CALLOC(np*nl, sizeof(float));

  FILE *fpi = fopenImage(infile, "rb");

  for (b=0; b<nb; ++b) {
    if (nb>0)
      asfPrintStatus("Deskewing band: %s\n", band_name[b]);

    // apply deskewing to this band
    for (i=0; i<nl; ++i) {
      get_float_line(fpi, meta, i + nl*b, ibuf);
      
      for (j=0; j<np; ++j) {
        int out_line = d ? i : i + lower[j];
        if (out_line >= 0 && out_line < nl)
          obuf[out_line*np+j] = ibuf[j];
      }
      
      asfLineMeter(i,nl);
    }

    // write out this band
    FILE *fpo = fopenImage(tmp_outfile, b>0 ? "ab" : "wb");
    put_float_lines(fpo, meta, b*nl, nl, obuf);
    FCLOSE(fpo);
  }

  FCLOSE(fpi);
  FREE(obuf);
  FREE(ibuf);
  FREE(lower);

  // if we output to a temporary file, now clobber the input
  if (do_rename) {
    rename(tmp_outfile, outfile);
  }

  FREE(tmp_outfile);

  // only need to update the deskewed flag in the metadata
  meta->sar->deskewed = 1;
  meta_write(meta, outfile);
}
