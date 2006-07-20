#include "asf.h"
#include "asf_meta.h"
#include "asf_endian.h"

#define VERSION 1.0

char *uc(char *string)
{
  char *out=(char *)MALLOC(sizeof(char)*strlen(string));
  int i;
  
  for (i=0; i<strlen(string); i++) out[i]=toupper(string[i]);
  out[i]='\0';
  
  return out;
}

static
void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s [-list] <input format> <output format> <input file> <output file>\n",
	name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"    input format:  Input format: srf, meta, leader\n"
	"    output format: Output format: shape, kml\n"
	"    input file:    Name of the input file\n"
	"    output file:   Basename of the output file.\n");
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"
	"   -list:          Input file name contains a list of filenames to be "
	"converted.\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   Converts metadata information into external vector formats\n");
 printf("\n"
	"Version: %.2f, ASF InSAR Tools\n"
	"\n",VERSION);
 exit(EXIT_FAILURE);
}

int main(int argc, char **argv)
{
  meta_parameters *meta;
  char informat[25], outformat[25], infile[255], outfile[255];
  extern int currArg; /* pre-initialized to 1; like optind */
  int listflag=0;
  
  /* parse command line */
  while (currArg < (argc-4)) {
    char *key = argv[currArg++];
    if (strmatch(key,"-list")) {
      listflag=1;
    }
    else {
      printf( "\n**Invalid option:  %s\n",argv[currArg-1]); 
      usage(argv[0]);
    }
  }
  if ((argc-currArg) < 4) {
    printf("Insufficient arguments.\n"); 
    usage(argv[0]);
  }
  
  sprintf(informat, "%s", argv[currArg]);
  sprintf(outformat, "%s", argv[currArg+1]);
  sprintf(infile, "%s", argv[currArg+2]);
  sprintf(outfile, "%s", argv[currArg+3]);
  
  asfSplashScreen (argc, argv);


  // Call library functions that get the work done
  if (listflag) {
    if (strcmp(uc(informat), "META")==0 && strcmp(uc(outformat), "SHAPE")==0) {
      asfPrintStatus("   Converting list of metadata files into shape files"
		     " ...\n\n");
      meta2shape_list(infile, outfile);
    }
    else if (strcmp(uc(informat), "META")==0 && strcmp(uc(outformat), "KML")==0) {
      asfPrintStatus("   Converting list of metadata files into kml files"
		     " ...\n\n");
      meta2kml_list(infile, outfile);
    }
  }
  else {
    if (strcmp(uc(informat), "META")==0 && strcmp(uc(outformat), "SHAPE")==0) {
      asfPrintStatus("   Converting metadata file into shape file ...\n\n");
      meta2shape(infile, outfile);
    }
    else if (strcmp(uc(informat), "META")==0 && strcmp(uc(outformat), "KML")==0) {
      asfPrintStatus("   Converting metadata file into kml file ...\n\n");
      meta = meta_read(infile);
      meta2kml(outfile, meta);
    }
  }

  return(0);
}

