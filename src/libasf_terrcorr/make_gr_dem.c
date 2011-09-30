#include <asf_terrcorr.h>
#include <stdio.h>
#include <assert.h>

static float *
read_dem(meta_parameters *meta_dem, const char *demImg)
{
  int ns = meta_dem->general->sample_count;
  int nl = meta_dem->general->line_count;
  float *demData = MALLOC(sizeof(float)*ns*nl);

  FILE *fp = FOPEN(demImg, "rb");

  int ii;
  for (ii=0; ii<nl; ++ii) {
    get_float_line(fp, meta_dem, ii, demData + ii*ns);
    asfLineMeter(ii,nl);
  }

  FCLOSE(fp);
  return demData;
}

static float
bilinear_interp(float *demData, int nl, int ns, double l, double s)
{
  if (l<0 || l>=nl || s<0 || s>=ns) {
    return 0;
  }

  int ix = (int)s;
  int iy = (int)l;

  float p00 = demData[ix   + ns*(iy  )];
  float p10 = demData[ix+1 + ns*(iy  )];
  float p01 = demData[ix   + ns*(iy+1)];
  float p11 = demData[ix+1 + ns*(iy+1)];

  float a00 = p00;
  float a10 = p10 - p00;
  float a01 = p10 - a00;
  float a11 = p00 - p01 - p10 + p11;

  return a00 + a10*(s-ix) + a01*(l-iy) + a11*(s-ix)*(l-iy);
}

int make_gr_dem(meta_parameters *meta, const char *demImg, const char *demMeta, int pad,
                const char *output_name)
{
  asfPrintStatus("Reading DEM...\n");
  meta_parameters *meta_dem = meta_read(demMeta);
  float *demData = read_dem(meta_dem, demImg);
  int dnl = meta_dem->general->line_count;
  int dns = meta_dem->general->sample_count;

  char *outImg = appendExt(output_name, ".img");
  char *outMeta = appendExt(output_name, ".meta");

  meta_parameters *meta_out = meta_copy(meta);
  meta_out->general->line_count += pad*2;
  meta_out->general->sample_count += pad*2;
  meta_out->general->start_line -= pad;
  meta_out->general->start_sample -= pad;

  int nl = meta_out->general->line_count;
  int ns = meta_out->general->sample_count;

  float *buf = MALLOC(sizeof(float)*ns);
  FILE *fpOut = FOPEN(outImg, "wb");

  asfPrintStatus("Creating ground range image...\n");

  int ii, jj;
  for (ii=0; ii<nl; ++ii) {
    for (jj=0; jj<ns; ++jj) {
      double lat, lon, line, samp;
      meta_get_latLon(meta_out, ii, jj, 0, &lat, &lon);
      meta_get_lineSamp(meta_dem, lat, lon, 0, &line, &samp);
      buf[jj] = bilinear_interp(demData, dnl, dns, line, samp);
    }
    asfLineMeter(ii,nl);
    put_float_line(fpOut, meta_out, ii, buf);
  }

  FCLOSE(fpOut);
  meta_write(meta_out, outMeta);

  FREE(buf);
  FREE(demData);
  FREE(outImg);
  FREE(outMeta);

  return TRUE;
}

