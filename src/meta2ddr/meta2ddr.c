#include "asf.h"
#include "asf_meta.h"
#include "las.h"

#define VERSION 1.0

void meta_write_old(meta_parameters *meta, const char *file_name);

int main(int argc, char **argv)
{
	char las_nameDDR[256];
	char las_nameMeta[256];
	char meta_name[256];
	meta_parameters *meta;
	struct DDR ddr;
	extern int currArg; /* from cla.h in asf.h... initialized to 1 */
	
/* Parse command line */
	if (argc-currArg < 2)
		{printf("Insufficient arguments.\n"); usage(argv[0]);}
	if (argc-currArg > 2)
		{printf("Excessive arguments.\n"); usage(argv[0]);}
	create_name(meta_name, argv[currArg], ".meta");
	create_name(las_nameMeta, argv[currArg+1], ".meta");
	create_name(las_nameDDR, argv[currArg+1], ".ddr");

/* Read .meta and fill meta structures */ 
	meta = meta_read(meta_name);

/* Fill ddr struct with valid data */
	meta2ddr(meta,&ddr);

/* Write stuff out old style */
	c_putddr(las_nameDDR, &ddr);
	meta_write_old(meta, las_nameMeta);

/* Clean and report */
	meta_free(meta);
	printf("***Wrote %s and %s from %s.\n",
	       las_nameDDR, las_nameMeta, meta_name);

	return 0;
}


void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s <meta_name> <las_name>\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   meta_name   Base name of the new style meta data.  I.E. if <meta_name> is\n"
	"                \"test1\", then \"test1.meta\" must be in the working directory\n"
 	"   las_name    Base name of the old style meta data.  I.E. if <las_name> is\n"
	"                \"test2\", then both \"test2.ddr\" and \"test2.meta\" must be\n"
	"                in the working directory\n");
printf("\n"
	"DESCRIPTION:\n"
	"   %s coverts new ASF style metadata to old ASF style metadata.\n"
	"   Old metadata being a LAS DDR file and a pre-version 1.10 meta file.\n"
	"   New metadata is a meta file that is version 1.10 or greater. Current\n"
	"   version is %.2f. Make sure that <las_name> and <meta_name> are\n"
	"   different. Otherwise the meta file that was read in will get written\n"
	"   over.\n",name,META_VERSION);
 printf("\n"
	"Version %.2f, ASF SAR Tools\n"
	"\n",VERSION);
 exit(EXIT_FAILURE);
}
