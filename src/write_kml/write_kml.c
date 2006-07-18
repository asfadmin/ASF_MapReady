#include "asf_reporting.h"
#include "asf_vector.h"
#include <stdio.h>
#include <stdlib.h>

// Print minimalistic usage info & exit
static void print_usage(void)
{
  asfPrintStatus("\n"
      "Usage: write_kml <filename>\n"
      "\n");
  exit(EXIT_FAILURE);
}

int main(int argc, char *argv[])
{
    if (argc != 2) print_usage();
    write_kml(argv[1]);
    exit(EXIT_SUCCESS);
}
