#include "asf.h"
#include "asf_meta.h"

#define VERSION 1.0

void usage(char *name);

int main (int argc, char **argv)
{
	char ceosName[256];
	char metaName[256];
	meta_parameters *meta;
	
	/* Parse command line */
	if (argc-currArg < 2)
		{printf("Insufficient arguments.\n"); usage(argv[0]);}
	if (argc-currArg > 2)
		{printf("Excessive arguments.\n"); usage(argv[0]);}
	strcpy(ceosName,argv[currArg]);
	strcpy(metaName,argv[currArg+1]);
	
	meta = meta_create(ceosName);
	meta_write(meta,metaName);
	meta_free(meta);
	
	exit(EXIT_SUCCESS);
}

void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s <ceosName> <metaName>\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   ceosName    Base name of the CEOS *.D and *.L files.\n"
 	"   metaName    Base name for the *.meta file to be created.\n");
printf("\n"
	"DESCRIPTION:\n"
	"   %s derives an ASF *.meta file from a CEOS *.L and *.D fileset.\n",
	name);
 printf("\n"
	"Version %.2f, ASF SAR Tools\n"
	"\n",VERSION);
 exit(EXIT_FAILURE);
}
