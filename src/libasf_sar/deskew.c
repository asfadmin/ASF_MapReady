#include "asf.h"
#include "asf_meta.h"
#include "asf_sar.h"
#include "geolocate.h"
#include "asf_raster.h"

// This is the guts of the "deskew" tool, moved here in to this
// library so we can call it from asf_terrcorr.

// Comments from the original deskew code ==>

//   Deskew uses the squint angle of an image along with the
//   look angle to determine the amount of parallelogram shift
//   skew that has been introduced in an image due to the doppler
//   centroid choosen during image processing.  It then remaps
//   the image (using bi-linear interpolation) to remove this skew.

// However, it doesn't look to me like bi-linear interpolation is
// taking place.  This probably a good thing, actually...

// Giving the same input & output filename is allowed, the
// input data will be overwritten.

// This code should actually work on data that is already deskewed,
// no changes are made, output matches input

static double calc_shift(meta_parameters *meta, double line, double samp)
{
  stateVector stVec = meta_get_stVec(meta, 0.0);
  GEOLOCATE_REC *g = init_geolocate_meta(&stVec, meta);

  double time, slant, dop;
  meta_get_timeSlantDop(meta, line, samp, &time, &slant, &dop);

  double look, yaw, fac;
  getLookYaw(g, slant, dop, &look, &yaw);
  fac = sin(look)*sin(yaw);
  free_geolocate(g);

  return fac;
}

void deskew(const char *infile, const char *outfile)
{
  meta_parameters *meta = meta_read(infile);

  int nl = meta->general->line_count;
  int np = meta->general->sample_count;
  int nb = meta->general->band_count;
  char **band_name = extract_band_names(meta->general->bands, nb);
  int band, line, samp, deskewed = meta->sar->deskewed != 0;

  if (!meta->sar)
    asfPrintError("Cannot deskew data without a sar block!\n");

  char *tmp_outfile;
  int do_rename = FALSE;
  if (strcmp(infile, outfile) == 0 && nb>1) {
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
  double fac = calc_shift(meta, 0, 0);

  // the "lower" array stores the required shifts, indexed by column
  // (the amount of shift is row-independent)
  int *lower = MALLOC(np * sizeof(int));
  for (samp=0; samp<np; ++samp)
    lower[samp] = (int) (fac*(double)samp);

  asfPrintStatus("Far-range shift amount: ");
  if (lower[np-1] > 0)
    asfPrintStatus("%d pixels down.\n", lower[np-1]);
  else
    asfPrintStatus("%d pixels up.\n", -lower[np-1]);

  float *ibuf = MALLOC(np * sizeof(float));
  float *obuf = CALLOC(np*nl, sizeof(float));

  FILE *fpi = fopenImage(infile, "rb");

  for (band=0; band<nb; ++band) {
    if (nb>1)
      asfPrintStatus("Deskewing band: %s\n", band_name[band]);

    // apply deskewing to this band
    for (line=0; line<nl; ++line) {
      get_float_line(fpi, meta, line + nl*band, ibuf);
      
      for (samp=0; samp<np; ++samp) {
        int out_line = deskewed ? line : line + lower[samp];
        if (out_line >= 0 && out_line < nl)
          obuf[out_line*np+samp] = ibuf[samp];
      }
      
      asfLineMeter(line,nl);
    }

    // write out this band
    FILE *fpo = fopenImage(tmp_outfile, band>0 ? "ab" : "wb");
    put_float_lines(fpo, meta, band*nl, nl, obuf);
    FCLOSE(fpo);
  }

  FCLOSE(fpi);
  FREE(obuf);
  FREE(ibuf);
  FREE(lower);

  // if we output to a temporary file, clobber the input
  if (do_rename)
    rename(tmp_outfile, outfile);
  FREE(tmp_outfile);

  // only need to update the deskewed flag in the metadata
  meta->sar->deskewed = 1;
  meta_write(meta, outfile);
  meta_free(meta);
}
