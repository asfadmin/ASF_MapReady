#include "asf.h"
#include "asf_meta.h"
#include "asf_raster.h"
#include "asf_license.h"

void usage()
{
  printf("Usage:\n\n");
  printf(" trim_wedges <infile> <outfile>\n\n");
  printf("Produces an outfile with the blackfill wedges removed.\n");
  printf("These wedges are typically present in geocoded SAR images.\n");
  exit(1);
}
 
int main(int argc, char *argv[])
{
  handle_common_asf_args(&argc, &argv, "trim_wedges");

  if (argc != 3) usage();
  trim_wedges(argv[1], argv[2]);  
  asfPrintStatus("\nDone.\n");
  return(0);
}
