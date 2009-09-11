#include "asf.h"
#include "asf_terrcorr.h"

#define VERSION 1.0

void usage(char *name)
{
  printf("\n"
	 "USAGE:\n"
	 "   %s <sarName> <mapDemName> <simAmpName> <sarDemName>\n",name);
  printf("\n"
	 "REQUIRED ARGUMENTS:\n"
	 "   <sarName>        SAR image\n"
	 "   <mapDemName>     map projected reference DEM\n"
	 "   <simAmpName>     simulated amplitude image\n"
	 "   <sarDemName>     slant range DEM\n"
	 "   All filenames require extensions.\n");
  printf("\n"
	 "DESCRIPTION:\n"
	 "   For images in a SAR geometry (ground range or slant range) it reprojects\n"
	 "   a DEM and simulates an amplitude image into SAR \n"
	 "   geometry using a map projected reference DEM. The mapping can be done\n"
	 "   using a polynomial or a least-square (default) fitting approach.\n"
	 "   For map projected SAR images it simulates an amplitude image for the\n"
	 "   reference DEM in its map projection.\n"
	 "   Matching the real and simulated amplitude image \n"
	 "   in frequency domain is used to determine the offsets to correct\n"
	 "   the geolocation.\n");
  printf("\n"
	 "Version %.2f, ASF SAR TOOLS\n"
	 "\n",VERSION);
  exit(1);
}


int main(int argc, char *argv[])
{
  char *sarFile, *demFile, *demSlant, *demSimAmp;
  
  printf("Date: %s\n",date_time_stamp());
  printf("Program: asf_check_geolocation\n\n");  

  if (argc!= 5) {printf("   Insufficient arguments.\n"); usage(argv[0]);}

  // Fetch required arguments
  sarFile = argv[currArg++];
  demFile = argv[currArg++];
  //inMaskFile = argv[currArg++];
  demSimAmp = argv[currArg++];
  demSlant = argv[currArg];

  asf_check_geolocation(sarFile, demFile, NULL, demSimAmp, demSlant);
  
  exit(0);
}
