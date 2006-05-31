#include <asf_raster.h>
#include <stdio.h>
#include <stdlib.h>
#include <asf.h>

int main(int argc, char *argv[])
{
  int addSpeckle = 1;

  if (argc != 3) {
    printf("Usage: shaded_relief <infile> <outfile>\n");
    exit(1);
  }

  shaded_relief(argv[1], argv[2], addSpeckle);
  exit(EXIT_SUCCESS);
}
