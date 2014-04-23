#include "asf_meta.h"
#include "asf.h"

char *op_str(int op_val)
{
  switch(op_val) {
    case 1:  return "=";
    case 2:  return "!=";
    case 3:  return ">";
    case 4:  return "<";
  }
  return "?";
}
 
int main(int argc, char *argv[])
{
  int in_arg=-1, out_arg=-1;
  float in_vals[256];
  float out_vals[256];
  int op[256];

  if (argc == 6) {
    in_arg = 4;
    out_arg = 5;

    op[0] = 1;
    op[1] = 0;

    in_vals[0] = atof(argv[2]);
    out_vals[0] = atof(argv[3]);

  } else if (argc == 4) {
    in_arg = 2;
    out_arg = 3;

    char line[256];
    int k=0;

    FILE *fp = FOPEN(argv[1],"r");
    while (fgets(line,255,fp) != NULL) {
      char c[3];
      c[2] = 0;
      double in, out;
      int n = sscanf(line, "%c%c %lf %lf\n", &c[0], &c[1], &in, &out);
      if (n != 4) {
        printf("Bad line: %s\n", line);
      } else {
             if (strcmp_case(c, "eq") == 0) { op[k] = 1; }
        else if (strcmp_case(c, "ne") == 0) { op[k] = 2; }
        else if (strcmp_case(c, "gt") == 0) { op[k] = 3; }
        else if (strcmp_case(c, "lt") == 0) { op[k] = 4; }
        else { printf("Bad line: %s\n", line); continue; }
        in_vals[k] = in;
        out_vals[k] = out;
        ++k;
        if (k>255) {
          asfPrintError("Exceeded max 256 values allowed in %s\n", argv[1]);
        } 
        op[k] = 0;
      }
    }
    FCLOSE(fp);
  }
  else {
    printf("Usage:\n");
    printf("  change_value <operation> <current value> <new value> <input file> <output file>\n\n");
    printf("The value of <operation> can be one of 'eq', 'ne', 'gt', or 'lt'\n\n");
    printf("Example:\nchange_value eq -9999 -10000 infile outfile\n\n");
    printf("Alternatively:\n");
    printf("  change_value <value map file> <input file> <output file>\n\n");
    printf("Example:\nchange_value values.txt infile outfile\n");
    printf("Example contents of values.txt:\n");
    printf("  lt -999 -10000\n");
    printf("  eq 0 -10000\n");
    printf("  eq -1 -10000\n\n");
    printf("The input file should be in ASF Internal format.\n");
    exit(1);
  }

  char *in_img = appendExt(argv[in_arg], ".img");
  char *in_meta = appendExt(argv[in_arg], ".meta");

  char *out_img = appendExt(argv[out_arg], ".img");
  char *out_meta = appendExt(argv[out_arg], ".meta");

  printf("Input image file: %s\n", in_img);
  printf("Output image file: %s\n", out_img);

  meta_parameters *meta = meta_read(in_meta);
  int i, j, k, n=0, ns=meta->general->sample_count;

  i=0;
  do {
    printf("Changing %s%f to %f\n", op_str(op[i]), in_vals[i], out_vals[i]);
    ++i;
  }
  while (op[i] != 0);

  FILE *ifp = FOPEN(in_img, "rb");
  FILE *ofp = FOPEN(out_img, "wb");

  float *buf = MALLOC(sizeof(float)*ns);

  for (i=0; i<meta->general->line_count; ++i) {
    get_float_line(ifp, meta, i, buf);
    for (j=0; j<ns; ++j) {
      k=0;
      int cond = FALSE;
      do {
        switch(op[k]) {
          case 1: cond = buf[j] == in_vals[k]; break;
          case 2: cond = buf[j] != in_vals[k]; break;
          case 3: cond = buf[j] > in_vals[k];  break;
          case 4: cond = buf[j] < in_vals[k];  break;
        }
        if (!cond) { ++k; }
      }
      while (op[k] != 0 && !cond);
     
      if (cond) {
        ++n;
        buf[j] = out_vals[k];
      }
    }
    put_float_line(ofp, meta, i, buf);
    asfLineMeter(i,meta->general->line_count);
  }

  FCLOSE(ifp);
  FCLOSE(ofp);
  FREE(buf);

  if (meta_is_valid_double(meta->general->no_data)) {
    k=0;
    int cond = FALSE;
    do {
      switch(op[k]) {
        case 1: cond = meta->general->no_data == in_vals[k]; break;
        case 2: cond = meta->general->no_data != in_vals[k]; break;
        case 3: cond = meta->general->no_data > in_vals[k];  break;
        case 4: cond = meta->general->no_data < in_vals[k];  break;
      }
      if (!cond) { ++k; }
    }
    while (op[k] != 0 && !cond);
    if (cond) {
      meta->general->no_data = out_vals[k];
    }
  }

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
