#include "libasf_meta.h"

#define VERSION 1.0

void usage(char *name)
{
  printf("\n"
         "USAGE:\n"
         "   %s  <meta_in> <version_in> <meta_out> <version_out>\n",name);
  printf("\n"
         "REQUIRED ARGUMENTS:\n"
         "   meta_in       Name of input metadata file.\n"
	 "   version_in    Version of input metadata file.\n"
	 "   meta_out      Name of output metadata file.\n"
	 "   version_out   Version of output metadata file.\n");
  printf("\n\n"
         "DESCRIPTION:\n"
         "   %s converts between different versions of metadata files.\n",
         name);
  printf("\n"
         "Version %.2f, ASF SAR Tools\n"
         "\n",VERSION);
  exit(EXIT_FAILURE);
}

int main(int argc, char *argv[])
{
  meta_parameters *meta=NULL;
  struct DDR *ddr=NULL;
  char metaIn[255], metaOut[255], versionIn[10], versionOut[10];

  /* Parse command line args */
  if (argc < 4) {
    printf("Insufficient arguments.\n"); 
    usage(argv[0]);
  }
  
  sprintf(metaIn, "%s", argv[1]);
  sprintf(versionIn, "%s", argv[2]);
  sprintf(metaOut, "%s", argv[3]);
  sprintf(versionOut, "%s", argv[4]);

  asf_meta_read(metaIn, versionIn, &meta, &ddr);
  asf_meta_write(meta, versionOut, ddr, metaOut);
     
  return(0);
}
