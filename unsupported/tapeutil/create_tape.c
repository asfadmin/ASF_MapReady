/*************************create_tape**********************
OVERVIEW:
	create_tape is a utility designed to write ASF
format product tapes.  To run this program, you need a Volume
Descriptor File (VDF), as well as a set of ASF products to write.

	A VDF is a mixed binary-and-ASCII format file which describes,
in terms of 360-byte records, the contents of an ASF tape.  
Specifically, it gives the names of each of the data (.D) and
metadata (.L) files which are to be written to tape.  The VDF is
in a CEOS-Standard format, and is copiously documented elsewhere.

	create_tape uses concat_vdf (another ASF STEP Lab
utility) to extract from the VDF a list of files to write to tape.
It then uses the standard UNIX utility dd to copy the VDF,
all the referenced files, and finally a Null Descriptor File (NDF)
to the tape.  If the VDF, NDF, or any of the referenced files cannot 
be found in the current directory, create_tape exits with
an error.

USAGE:

  create_tape <output device name> <VDF file 1> [ <VDF file 2> [...] ]
  
	<output device name>  The destination tape device (e.g. /dev/rmt/1n)
		Note that you probably want a device name that ends in 
		"n"-- otherwise each file will overwrite the previous
		one on the tape.
		
       <VDF file 1> the .VDF file, with extention, which specifies
		the files to write to tape.  The files
		listed in the VDF are read from the current directory
		and written to tape.  A file with this base name, 
		but an extention of .NDF, must also be present in the 
		directory (this is the Null Volume File).
		
	As many of these VDFs can be specified as desired.

AUTHOR: Orion Lawlor, ASF STEP Lab, 1997.
*******************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
void usage(void)
{
	printf("create_tape: write an ASF product set to tape.\n");
	printf("usage: \n\tcreate_tape <device name> [ -<VDF list file> | <VDF file 1> [ more VDFs [...]]]\n");
	printf("   -<VDF list file> a text file containing VDF file names.\n\
	    <VDF file> the .VDF file, with extention, which specifies\n\
		the files to write to tape (e.g. 9593001.VDF).  The files\n\
		listed in the VDF are read from the current directory\n\
		and written to tape.  A file with the same base name, \n\
		but an extention of .NDF, must also be present in the \n\
		directory (this is the Null Volume File).\n");
	printf("   <device name>  The destination tape device (e.g. /dev/rmt/1n)\n");
	printf("\n  Developed in the ASF STEP Lab, 6/1997.\n");
	exit(1);
}
#define execute(cmd) \
	if (0==strcmp(tapeDriveName,"/dev/null")) \
		printf("\t\t*>%s\n",cmd); \
	else \
		Execute(cmd);
void Execute(const char *cmd);
void fileExists(const char *fname,const char *errorMessage);



void addFileToTapelist(const char *filename, int blockSize);
char *getFileFromTapelist(int *blockSize);

main(int argc,char **argv)
{
	int i;
	const int vdfBlockSize=360;
	char *vdfcatOutfile="vdfcatOutfile",*tapeDriveName=argv[1];
	char cmd[10000];
	char *vdfName="temp.VDF",ndfName[255],*vdfListFile;
	char nameLine[255];
	int thisVolume,totalVolumes;
	FILE *getNamesF;
	if (argc<3)
		usage();
/* Get the VDF and NDF names.*/
	if (argv[2][0]=='-')
	{
		FILE *fileList=fopen(&argv[2][1],"r");
		if (fileList==NULL)
			{printf("Couldn't open file list %s.\n",fileList);exit(1);}
		fscanf(fileList,"%s",ndfName);
	}
	else
		strcpy(ndfName,argv[2]);
	strtok(ndfName,".");
	strcat(ndfName,".NDF");
	fileExists(ndfName,"Cannot open ndf %s.\n");
/* Get rid of any output files that already exist */
	sprintf(cmd,"/bin/rm -f %s",vdfcatOutfile);
	system(cmd);
/* Call concat_vdf */
	sprintf(cmd,"concat_vdf %s ",vdfName);
	i=2;
	if (argv[2][0]=='-')
	/*The user passed a vdf list file.*/
		strcat(cmd,argv[2]);
	else
	/*The user passed a bunch of file names.*/
		while (i<argc)
			strcat(strcat(cmd,argv[i++])," ");
	strcat(cmd," > ");
	strcat(cmd,vdfcatOutfile);
	Execute(cmd);
	fileExists(vdfcatOutfile,"Cannot open the output of concat_vdf-- %s\n");
/* Rewind the tape */
	sprintf(cmd,"mt -f %s rewind",tapeDriveName);
	execute(cmd);
/* Write out our new VDF */
	sprintf(cmd,"dd if=%s of=%s bs=%i",vdfName,tapeDriveName,vdfBlockSize);
	execute(cmd);
/* Go through the output of concat_vdf, writing out each file we find. */
	{
	  FILE *catvdfFile;
	  char line[255];
	  char tapeFileName[255];
	  int fileBlockSize;
	  catvdfFile=fopen(vdfcatOutfile,"r");
	  if (catvdfFile==NULL)
	  {
	  	printf("Cannot open %s for reading!\n",vdfcatOutfile);
	  	exit(1);
	  }
	  while (NULL!=(fgets(line,255,catvdfFile)))
	  {
	  	sscanf(line,"%s %i",tapeFileName,&fileBlockSize);
		sprintf(cmd,"dd if=%s of=%s bs=%i",tapeFileName,tapeDriveName,fileBlockSize);
		execute(cmd);
	  }
	  fclose(catvdfFile);
	}
/* Finally, copy the NDF over. */
	sprintf(cmd,"dd if=%s of=%s bs=%i",ndfName,tapeDriveName,vdfBlockSize);
	execute(cmd);
/* Dispose of our new VDF and the output file */
	sprintf(cmd,"/bin/rm %s %s",vdfName,vdfcatOutfile);
	Execute(cmd);
/* And rewind the tape. */
	sprintf(cmd,"mt -f %s rewind",tapeDriveName);
	execute(cmd);
	return (0);
}


void Execute(const char *cmd)
{
	int returnCode;
	printf("%s\n",cmd);
	returnCode=system(cmd);
	if (0!=returnCode)
	{
		printf("When create_product_tape was executing this system command: \n\
\t%s\nIt returned the error code %i.\n",cmd,returnCode);
		exit(1);
	}
}


void fileExists(const char *fname, const char *errorMessage)
{
	FILE *f;
	f=fopen(fname,"r");
	if (NULL==f)
		{fprintf(stderr,"create_tape fatal error:\n\t");fprintf(stderr,errorMessage,fname);exit(1);}
	fclose(f);
}
