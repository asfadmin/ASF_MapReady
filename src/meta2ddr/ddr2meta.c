#include "asf.h"
#include "asf_meta.h"
#include "las.h"
#include "ddr.h"

#define VERSION 1.0

int main(int argc, char **argv)
{
	char las_name[256];
	char meta_name[256];
	meta_parameters *meta;
	int ii;
	
/* Parse command line */
	currArg=1;
	if (argc-currArg < 2)
		{printf("Insufficient arguments.\n"); usage(argv[0]);}
	if (argc-currArg > 2)
		{printf("Excessive arguments.\n"); usage(argv[0]);}
	strcpy(las_name, argv[currArg]);
	strcpy(meta_name,argv[currArg+1]);
	
/* Read .ddr & .meta file info & put into meta structures */ 
	meta = meta_read(las_name);

/* write it out new style */
	meta_write(meta, meta_name);

/* Clean and report */
	meta_free(meta);
	printf("***Wrote %s.meta from %s.ddr and %s.meta.\n",
	       meta_name,las_name,las_name);

	return 0;
}


void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s <las_name> <meta_name>\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   las_name    Base name of the old style meta data.  I.E. if <las_name> is\n"
	"                \"test1\", then both \"test1.ddr\" and \"test1.meta\" must be\n"
	"                in the working directory\n"
	"   meta_name   Base name of the new style meta data.  I.E. if <meta_name> is\n"
	"                \"test2\", then \"test2.meta\" must be in the working directory\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   %s coverts old ASF style metadata to new ASF style metadata.\n"
	"   Old metadata being a LAS DDR file and a pre-version 1.10 meta file.\n"
	"   New metadata is a meta file that is version 1.10 or greater. Current\n"
	"   version is %.2f. Make sure that <las_name> and <meta_name> are\n"
	"   different.  Otherwise the meta file that was read in will get written\n"
	"   over.\n",name,META_VERSION);
 printf("\n"
	"Version %.2f, ASF SAR Tools\n"
	"\n",VERSION);
 exit(EXIT_FAILURE);
}
