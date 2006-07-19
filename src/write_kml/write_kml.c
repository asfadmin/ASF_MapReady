#include "asf_reporting.h"
#include "asf_vector.h"
#include <stdio.h>
#include <stdlib.h>

// Print minimalistic usage info & exit
static void print_usage(void)
{
    asfPrintStatus("\n"
      "Usage: write_kml <metadata filename> <metadata filename> ...\n"
      "\n"
      "  Creates a file with the same basename and with a .kml extension,\n"
      "  which may be loaded into Google Earth.  All that is required is\n"
      "  a metadata file.  Only the basename needs to be given.\n\n"
      "  More than one file may be listed, e.g.:  write_kml *.meta\n\n");
    exit(EXIT_FAILURE);
}

int main(int argc, char *argv[])
{
    int i;
    if (argc == 0) print_usage();
    for (i=1; i<argc; ++i) {
        asfPrintStatus("Generating kml file for: %s\n", argv[i]);
        write_kml(argv[i]);
    }
    exit(EXIT_SUCCESS);
}
