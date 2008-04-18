#include "asf.h"
#include "asf_meta.h"
#include "asf_raster.h"
#include "asf_license.h"

void usage()
{
  printf("Usage:\n\n");
  printf(" trim_zeros [-u] [-notop] [-noleft] <infile> <outfile>\n\n");
  printf("       -u:      Update the metadata with new geolocation info.\n");
  printf("       -notop:  Do not trim zero fill at the top or bottom of\n");
  printf("                the image.\n");
  printf("       -noleft: Do not trim zero fill at the left or right of\n");
  printf("                the image.\n\n");
  printf("Running with -u means that the original file has good metadata\n");
  printf("and you just want to eliminate black borders, and update the\n");
  printf("metadata accordingly.\n\n");
  printf("Running without -u means that the original file has incorrect\n");
  printf("metadata, in that it doesn't correctly account for the zero\n");
  printf("fill at the edge of the image. E.g., the 'slant range to first\n");
  printf("pixel' value measures the slant range not to the pixel at sample\n");
  printf("number zero, but the first non-zero pixel.  So, in this case,\n");
  printf("after eliminating the zero fill, the metadata values are correct\n");
  printf("and the only values that need updating are the image sizes.\n\n");
  printf("DQ Test Mode: **FOR USE BY FRANZ ONLY!**\n");
  printf(" trim_zeros -dqtest <infile>\n\n");
  printf("Runs trim_zeros 3 times:\n");
  printf(" trim_zeros -notop <infile> <infile>_top\n");
  printf(" trim_zeros -noleft <infile> <infile>_left\n");
  printf(" trim_zeros <infile> <infile>_both\n\n");
  exit(1);
}
 
int main(int argc, char *argv[])
{
  handle_common_asf_args(&argc, &argv, "trim_zeros");

  int dqtest = extract_flag_options(&argc, &argv, "-dqtest", NULL);
  if (dqtest) {
    char *top = appendToBasename(argv[1], "_top");
    char *left = appendToBasename(argv[1], "_left");
    char *none = appendToBasename(argv[1], "_none");
    
    asfPrintStatus("\nCreating %s...\n", top);
    trim_zeros_ext(argv[1], top, FALSE, FALSE, TRUE);

    asfPrintStatus("\nCreating %s...\n", left);
    trim_zeros_ext(argv[1], left, FALSE, TRUE, FALSE);

    asfPrintStatus("\nCreating %s...\n", none);
    trim_zeros_ext(argv[1], none, FALSE, TRUE, TRUE);

    FREE(top);
    FREE(left);
    FREE(none);
  } else {
    int update_meta = extract_flag_options(&argc, &argv, "-u", NULL);

    int do_top = !extract_flag_options(&argc, &argv, "-notop", NULL);
    int do_left = !extract_flag_options(&argc, &argv, "-noleft", NULL);

    if (argc < 3) usage();
    if (argc > 3) {
      printf("*** Too many arguments.\n\n");
      usage();
    }
 
    /* Call library function */
    trim_zeros_ext(argv[1], argv[2], update_meta, do_top, do_left);  
  }

  asfPrintStatus("\nDone.\n");
  return(0);
}
