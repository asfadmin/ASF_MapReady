#include "asf.h"
#include "asf_meta.h"
#include "asf_raster.h"
#include "asf_license.h"

void usage()
{
  printf("Usage:\n\n");
  printf(" kernel -type <kernel_type> <infile> <outfile>\n\n");
  printf("Produces an outfile with the specified kernel applied.\n");
  printf("The given kernel is applied to all pixels in the input.\n");
  printf("image, to produce the output.\n\n");
  printf("Valid kernel types:\n");
  printf("  SOBEL   An edge detection kernel\n");
  exit(1);
}
 
int main(int argc, char *argv[])
{
  handle_common_asf_args(&argc, &argv, "kernel");

  char type[255];
  int type_given =
     extract_string_options(&argc, &argv, type, "-type","--type","-t",NULL);

  filter_type_t ktype;
  int size;

  // these are not used for most fiters
  float damping = 0;
  int nLooks = 0;
 
  if (strcmp_case(type, "SOBEL") == 0) {
    ktype = SOBEL;
    size = 3;
  }
  else {
    asfPrintError("Unrecognized kernel type: %s\n");
    return -1; // not reached
  }

  if (argc != 3 || !type_given) usage();
  kernel_filter(argv[1], argv[2], ktype, size, damping, nLooks);
  asfPrintStatus("\nDone.\n");
  return(0);
}
