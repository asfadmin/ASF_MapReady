#include "asf_sar.h"
#include "asf.h"
#include "asf_license.h"
#include "to_sr_help.h"

#include <stdio.h>
#include <stdlib.h>

int main(int argc,char *argv[])
{
  float srPixSize= -1;   /* output pixel size             */

  char  infile[256];     /* Input file name               */
  char  outfile[256];    /* Output file name              */

  if (argc > 1) {
    check_for_help(argc, argv);
    handle_common_asf_args(&argc, &argv, TOOL_NAME);
  }
  if ((argc != 5 && argc != 3) || (argc == 5 && strcmp(argv[1], "-p") != 0)) {
    asfPrintStatus("**Not enough arguments\n");
    usage();
    return 1;
  }

  if (strcmp(argv[1],"-p") == 0) {
    create_name(infile,argv[3],".img");
    create_name(outfile,argv[4],".img");
  } else {
    create_name(infile,argv[1],".img");
    create_name(outfile,argv[2],".img");
  }

  asfPrintStatus("Converting to slant range...\n");

  if (argc == 5) {
    srPixSize = atof(argv[2]);
    to_sr_pixsiz(infile, outfile, srPixSize);
  }
  else {
    to_sr(infile, outfile);
  }

  exit(EXIT_SUCCESS);
}

