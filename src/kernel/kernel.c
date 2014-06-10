#include "asf.h"
#include "asf_meta.h"
#include "asf_raster.h"
#include "asf_license.h"

void usage()
{
  printf("Usage:\n\n");
  printf(" kernel -type <kernel_type> [ -size <kernel size> ] [ -nlooks <value> ]\n");
  printf("        [ -dampling <damping factor> ] <infile> <outfile>\n\n");
  printf("Produces an outfile with the specified kernel applied.\n");
  printf("The given kernel is applied to all pixels in the input.\n");
  printf("image, to produce the output.\n\n");
  printf("Options:\n\n");
  printf("  Note that not all options apply to all kernel types!\n\n");
  printf("  -size     Kernel size to use, must be odd.\n");
  printf("  -nlooks   Number of looks in the radar image.\n");
  printf("  -damping  Exponential damping factor.\n\n");
  printf("Valid kernel types:\n");
  printf("  Name      Description (options used)\n");
  printf("  ----      --------------------------\n");
  printf("  SOBEL     An edge detection kernel (none)\n");
  printf("  AVERAGE   Averages pixels (size, default is 3)\n");
  printf("\n");
  exit(1);
}
 
int main(int argc, char *argv[])
{
  handle_common_asf_args(&argc, &argv, "kernel");

  char type[255];
  int type_given =
     extract_string_options(&argc, &argv, type, "-type","--type","-t",NULL);

  int size=0;
  int size_given =
     extract_int_options(&argc, &argv, &size, "-size","--size","-kernel-size",
                         "--kernel-size","-k","-s",NULL);

  double damping=0.0;
  int damping_given =
     extract_double_options(&argc, &argv, &damping, "-damping","--damping",
                         "-damping-factor","--damping-factor","-d",NULL);

  int nLooks=0;
  int looks_given =
     extract_int_options(&argc, &argv, &nLooks, "-looks","--looks","-nlooks",
                         "--nlooks","-n","-l",NULL);

  filter_type_t ktype;

  if (strcmp_case(type, "AVERAGE") == 0 || strcmp_case(type, "AVG") == 0) {
    ktype = AVERAGE;
    if (!size_given) {
      asfPrintStatus("Kernel size not specified, defaulting to 3.");
      size = 3;
    }
    if (size%2 != 1)
      asfPrintError("Kernel size must be odd.\n");
    if (damping_given) asfPrintWarning("Damping factor option was ignored.\n");
    if (looks_given) asfPrintWarning("Number of looks option was ignored.\n");
  }
  else if (strcmp_case(type, "GAUSSIAN") == 0) {
    ktype = GAUSSIAN;
    if (!size_given) {
      asfPrintStatus("Kernel size not specified, defaulting to 3.");
      size = 3;
    }
    if (size%2 != 1)
      asfPrintError("Kernel size must be odd.\n");
    if (damping_given) asfPrintWarning("Damping factor option was ignored.\n");
    if (looks_given) asfPrintWarning("Number of looks option was ignored.\n");
  }
  else if (strcmp_case(type, "SOBEL") == 0) {
    ktype = SOBEL;
    size = 3;
    if (size_given) asfPrintWarning("Kernel size option was ignored.\n");
    if (damping_given) asfPrintWarning("Damping factor option was ignored.\n");
    if (looks_given) asfPrintWarning("Number of looks option was ignored.\n");
  }
  else {
    if (type_given)
      asfPrintError("Unrecognized kernel type: %s\n", type);
    else
      usage();
    return -1; // not reached
  }

  if (argc != 3 || !type_given) usage();
  kernel_filter(argv[1], argv[2], ktype, size, damping, nLooks);
  asfPrintStatus("\nDone.\n");
  return(0);
}
