#include "asf.h"
#include "asf_raster.h"
#include <assert.h>

static float prev_col_result = -99999.99999;
static int prev_col_total = -1;

static float filter(      /****************************************/
    float *inbuf,         /* input image buffer                   */
    int    nl,            /* number of lines for inbuf            */
    int    ns,            /* number of samples per line for inbuf */
    int    y,             /* line number in the image             */
    int    x,             /* sample in desired line               */
    int    nsk)           /* number of samples in kernel          */
{                         /****************************************/
  // For pixels other than the first, use the saved result for the pixel
  // right before this one in the same row, subtract that row that moved
  // off the window, add the row that moved into the window
  if (x>0) {

    //assert(prev_col_result != -99999.99999);
    assert(prev_col_total != -1);

    int half = (nsk-1)/2;
    float kersum = prev_col_result*prev_col_total;
    int left = x-half-1;
    int include_left = left>=0;
    int right = x+half;
    int include_right = x+half<ns;
    int total = prev_col_total;
    int i;

    if (include_left) {
      for (i = 0; i < nl; i++) {
        if (inbuf[left]!=0) {
          kersum -= inbuf[left];
          --total;
        }
        left += ns;
      }
    }

    if (include_right) {
      for (i = 0; i < nl; i++) {
        if (inbuf[right]!=0) {
          kersum += inbuf[right];
          ++total;
        }
        right += ns;
      }
    }

    if (total != 0)
      kersum /= (float)total;

    prev_col_result = kersum;
    prev_col_total = total;

    return kersum;
  }

  // Otherwise, do the full calculation (this should occur only for the
  // first pixel in each row)
  else {
    float  kersum =0.0;                    /* sum of kernel       */
    int    half   =(nsk-1)/2,              /* half size kernel    */
           total  =0,                      /* valid kernel values */
           i, j;                           /* loop counters       */

    int upper = x+half;
    if (upper >= ns) upper = ns-1;

    int lower = x-half;
    if (lower < 0) lower = 0;
    int wid = upper-lower+1;

    int base = lower;
    for (i = 0; i < nl; i++)
    {
      for (j = lower; j <= upper; j++)
      {
        if (inbuf[base] != 0)
        {
          kersum += inbuf[base];
          total++;
        }
        base++;
      }

      base += ns;
      base -= wid;
    }

    if (total != 0)
      kersum /= (float) total;

    prev_col_result = kersum;
    prev_col_total = total;

    return (kersum);
  }
}

static const char *edge_strat_to_string(edge_strategy_t edge_strategy)
{
  switch (edge_strategy) {
    case EDGE_TRUNCATE:
      return "Truncate";
    default:
      return "???";
  }
}

int smooth(const char *infile, const char *outfile, int kernel_size,
           edge_strategy_t edge_strategy)
{
  char *in_img = appendExt(infile, ".img");
  char *out_img = appendExt(outfile, ".img");
  char *out_meta_name = appendExt(outfile, ".meta");

  if (kernel_size<=2)
    asfPrintError("Kernel size must be odd and >= 3.\n");

  // must have an odd kernel size
  if (kernel_size%2 == 0) {
    --kernel_size;
    asfPrintWarning("An odd kernel size is required.\n"
                    "Using %d instead of the requested %d.\n",
                    kernel_size, kernel_size+1);
  }

  int half = (kernel_size-1)/2;

  asfPrintStatus("  Kernel size is %d pixels.\n", kernel_size);

  if (edge_strategy != EDGE_TRUNCATE)
    asfPrintError("Smooth: Unsupported edge strategy: %s (%d)\n",
                  edge_strategy, edge_strat_to_string(edge_strategy));

  asfPrintStatus("  Edge strategy: %s\n", edge_strat_to_string(edge_strategy));

  meta_parameters *metaIn = meta_read(infile);
  meta_parameters *metaOut = meta_read(infile);
  int nl = metaIn->general->line_count;
  int ns = metaIn->general->sample_count;

  float *inbuf = (float *) MALLOC (kernel_size*ns*sizeof(float));
  float *outbuf = (float *) MALLOC (ns*sizeof(float));

  char **band_name = extract_band_names(metaIn->general->bands,
                                        metaIn->general->band_count);

  FILE *fpin = fopenImage(in_img, "rb");

  int ii, jj, kk;
  for (kk = 0; kk < metaIn->general->band_count; ++kk) {
    if (metaIn->general->band_count != 1)
      asfPrintStatus("Smoothing band: %s\n", band_name[kk]);

    FILE *fpout = fopenImage(out_img, kk==0 ? "wb" : "ab");

    for (ii=0; ii<nl; ++ii) {

      // figure out which window in the image we need to read
      int start_line = ii - half;
      if (start_line < 0) start_line = 0;

      int n_lines = kernel_size;
      if (nl < kernel_size + start_line)
        n_lines = nl-start_line;

      // adjust for the band number
      start_line += kk*nl; 

      if (ii==0) {
        // read the window
        get_float_lines(fpin, metaIn, start_line, n_lines, inbuf);
      }
      else {
        // already read in most of these lines -- shift items in the buffer
        if (start_line > 0)
          for (jj = 0; jj < n_lines-1; ++jj)
            memcpy(inbuf + jj*ns, inbuf + (jj+1)*ns, ns*sizeof(float));
        int new_line = start_line + n_lines - 1;
        if (new_line < nl)
          get_float_line(fpin, metaIn, new_line, inbuf + (n_lines-1)*ns);
      }

      // apply the smoothing
      for (jj = 0; jj < ns; jj++)
        outbuf[jj] = filter(inbuf,n_lines,ns,ii,jj,kernel_size);

      put_float_line(fpout, metaOut, ii, outbuf);
      asfLineMeter(ii,nl);
    }

    FCLOSE(fpout);
  }
  FCLOSE(fpin);

  // metadata does not need any changes
  meta_write(metaOut, out_meta_name);

  // clean up
  for (ii=0; ii < metaIn->general->band_count; ii++)
    FREE(band_name[ii]);
  FREE(band_name);

  meta_free(metaOut);
  meta_free(metaIn);

  FREE(inbuf);
  FREE(outbuf);

  free(in_img);
  free(out_img);
  free(out_meta_name);

  return 0;
}

