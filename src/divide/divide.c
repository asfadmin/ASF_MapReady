#include "asf.h"
#include "asf_meta.h"

#include <stdio.h>
#include <stdlib.h>

#include <asf_license.h>
#include <asf_contact.h>

int main(int argc,char *argv[])
{
  char  infile[256];          // Input file name                         
  char  outfile[256];         // Output file name                        
  float cutoff = -999;        // Height below which we will replace           
  double fac = 1;

  if (argc != 4) {
      printf("\n**Not enough arguments (need 3, got %d).\n",argc);
      return 1;
  }

  fac = atof(argv[3]);
  asfPrintStatus("Dividing by: %f\n", fac);

  if (!quietflag)
      asfSplashScreen(argc, argv);

  create_name(infile,argv[currArg],".img");
  create_name(outfile,argv[currArg+1],".img");

  meta_parameters *meta = meta_read(infile);

  int i,j;
  float *buf = MALLOC(meta->general->sample_count * sizeof(float));
  FILE *fp = FOPEN(infile, "r");
  FILE *ofp = FOPEN(outfile, "w");

  for (i=0; i<meta->general->line_count; ++i) {
    get_float_line(fp, meta, i, buf);
    for (j=0; j<meta->general->sample_count; ++j) {
      if (!meta_is_valid_double(buf[j])) buf[j] = 0.0;
      buf[j] /= fac;
    }
    put_float_line(ofp, meta, i, buf);
  }

  meta_write(meta, outfile);

  fclose(fp);
  fclose(ofp);
    
  asfPrintStatus("Done.\n");
  exit(EXIT_SUCCESS);
}

