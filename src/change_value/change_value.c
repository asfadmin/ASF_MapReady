#include "asf_meta.h"

int main(int argc, char *argv[])
{
  if (argc != 5) {
    printf("Usage:\n");
    printf("  change_value <current value> <new value> <input file> <output file>\n\n");
    printf("Example:\nchange_value -9999 -10000 infile outfile\n\n");
    printf("The input file should be in ASF Internal format.\n");
    exit(1);
  }

  char *in_img = appendExt(argv[3], ".img");
  char *in_meta = appendExt(argv[3], ".meta");

  char *out_img = appendExt(argv[4], ".img");
  char *out_meta = appendExt(argv[4], ".meta");

  float in_val = atof(argv[1]);
  float out_val = atof(argv[2]);

  printf("Input image file: %s\n", in_img);
  printf("Output image file: %s\n", out_img);
  printf("Changing %f to %f\n", in_val, out_val);

  meta_parameters *meta = meta_read(in_meta);

  FILE *ifp = FOPEN(in_img, "rb");
  FILE *ofp = FOPEN(out_img, "wb");

  int n=0;
  int i, j, ns=meta->general->sample_count;
  float *buf = MALLOC(sizeof(float)*ns);

  for (i=0; i<meta->general->line_count; ++i) {
    get_float_line(ifp, meta, i, buf);
    for (j=0; j<ns; ++j) {
      if (buf[j] == in_val) { ++n; buf[j] = out_val; }
    }
    put_float_line(ofp, meta, i, buf);
    asfLineMeter(i,meta->general->line_count);
  }

  if (meta_is_valid_double(meta->general->no_data) &&
      meta->general->no_data == in_val)
  {
     meta->general->no_data = out_val;
  }

  FCLOSE(ifp);
  FCLOSE(ofp);

  FREE(buf);
  meta_write(meta, out_meta);
  meta_free(meta);
  if (n==0) {
    printf("No pixel values were changed!\n");
  } else {
    printf("%d pixel value%s changed.\n", n, n==1?"":"s");
  }
  printf("Done.\n");

  return 0;
}
