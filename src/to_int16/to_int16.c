#include "asf.h"
#include "asf_meta.h"
#include "asf_raster.h"
#include "asf_license.h"

void usage()
{
  printf("Usage:\n\n");
  printf(" to_int16 <infile> <outfile>\n\n");
  printf("Produces an outfile that is 16-bit integer.\n");
  exit(1);
}
 
int main(int argc, char *argv[])
{
  handle_common_asf_args(&argc, &argv, "to_int16");

  if (argc != 3) usage();

  char *in = appendExt(argv[1], ".img");
  char *out = appendExt(argv[2], ".img");

  FILE *fpIn = FOPEN(in, "rb");
  FILE *fpOut = FOPEN(out, "wb");

  meta_parameters *metaIn = meta_read(in);
  meta_parameters *metaOut = meta_read(in);
  metaOut->general->data_type = INTEGER16;

  int i,nl = metaIn->general->line_count;
  int ns = metaIn->general->sample_count;
  float *buf = MALLOC(sizeof(float)*ns);

  for (i=0; i<nl; i++) {
    get_float_line(fpIn, metaIn, i, buf);
    put_float_line(fpOut, metaOut, i, buf);
    asfLineMeter(i,nl);
  }

  FCLOSE(fpIn);
  FCLOSE(fpOut);
  meta_free(metaIn);
  meta_write(metaOut, out);
  meta_free(metaOut);
  FREE(buf);

  asfPrintStatus("\nDone.\n");
  return(0);
}
