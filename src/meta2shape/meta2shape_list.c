#include "asf_reporting.h"
#include "asf_vector.h"
#include <stdio.h>
#include <stdlib.h>

// Print minimalistic usage info & exit
static void print_usage(void)
{
  asfPrintStatus("\n"
      "Usage: meta2shape_list <file list> <shapeFile>\n"
      "\n");
  exit(EXIT_FAILURE);
}

int main(int argc, char *argv[])
{
    if (argc != 3) print_usage();
    meta2shape_list(argv[1], argv[2]);
    exit(EXIT_SUCCESS);
}
