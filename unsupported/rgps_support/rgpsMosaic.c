/*****************************************
rgpsMosaic:
	Concatenates scaled-down versions of revolution
and 3-day snapshot mosaics for the RGPS project.

Version	Date	Author
1.0	8/98	O. Lawlor

*/
#include "asf.h"
#include <math.h>
#include "ceos.h"
#include "geolocate.h"
#include "imageUtil.h"

/************** CONFIGURATION SECTION ******************/

/*Should we check duplicate frames for bad dopplers,
even when a newer (reprocessed) version exists?*/
#define checkDupsAnyway 1

/*Write little text caption on each image?*/
#define writeCaption 1

/*Maximum # of images we ever expect to have to deal with.*/
#define MAXIMG 10000

/*Pixel size (m) at which to build the mosaic.*/
#define mosaicPixelSize 500

/*Working directory, where files are built.*/
#define workDir "work/"

/*Coastline directory:*/
#define coastDir "/export/vol08step/olawlor/rgps_support/"

/**********************************
Execute:
	Runs given command on the system, 
checking it's error code.
*/
void execute(char *cmd)
{
	int ret;
	printf("Running:\n\t>> %s\n",cmd);
	fflush(stdout);
#if 1
	ret=system(cmd);
	if (ret!=0)
	{
		printf("Error! Command '%s' returned error code %d!\n",cmd,ret);
		exit(1);
	}
#endif
}

/*********************************************************
main:
	Read each given frame, checking for duplicates.
	Decide whether frames are good or bad.
	Output results.
*/
int main(int argc,char *argv[])
{
	int nImages=0;
	image *images[MAXIMG];
	
	int nRevs=0;
	int revs[MAXIMG];
	int rev;
	
	int i;
	char cmd[1024],newName[255];
	if (argc<2)
		{printf("Usage: rgpsMosaic <files...>\n"
		"Creates revolution, then cycle mosaics of the\n"
		"given files.\n"
		"ASF RGPS Tools, 1998.  Version 1.0\n");exit(1);}

/*Read in image files, checking for duplicate frames.*/
	for (i=1;i<argc;i++)
	{
		image *img=readImage(argv[i]);
		#if !checkDupsAnyway
		if (!dupImage(img,images,nImages))
		#endif
			images[nImages++]=img;
	}
	sprintf(cmd,"/bin/mkdir %s > /dev/null 2>&1",workDir);system(cmd);

/*Figure out which revolutions are present.*/
	for (i=0;i<nImages;i++)
	{
		int revAlreadyPresent=0;
		for (rev=0;rev<nRevs;rev++)
			if (revs[rev]==images[i]->rev)
				revAlreadyPresent=1;
		if (!revAlreadyPresent)
		/*Add this rev. to the list of revs.*/
			revs[nRevs++]=images[i]->rev;
	}
	
/*Build revolution mosaics, using concat.*/
	for (rev=0;rev<nRevs;rev++)
	{
	/*Ingest and resample each frame in the revolution.*/
		for (i=0;i<nImages;i++)
			if (revs[rev]==images[i]->rev)
			{
			/*Ingest frame to temp. image.*/
				char inName[255],caption[255];
				strcpy(inName,images[i]->name);
				strtok(inName,".");
				sprintf(cmd,"sarin %s %stmp_las\n",inName,workDir);
				execute(cmd);
				
			/*Resample temp. image.*/
#define createResName(resName,img) sprintf(resName,"frame_%d_%d",img->rev,img->frame)
				createResName(newName,images[i]);
				sprintf(cmd,"resample %stmp_las %s%s %d\n",workDir,workDir,
			#if writeCaption
					"tmp_lil",
			#else
					newName,
			#endif
					
					mosaicPixelSize);
				execute(cmd);
			
			#if writeCaption	
			/*Write little caption on image center.*/
				sprintf(caption,"\"Rev %d/Frame %d\"",images[i]->rev,images[i]->frame);
				sprintf(cmd,"las_text %stmp_lil.img %s%s.img %s 9 255 1\n",workDir,workDir,newName,caption);
				execute(cmd);
				sprintf(cmd,"/bin/rm %stmp_lil.img %stmp_lil.ddr\n",workDir,workDir);
				execute(cmd);
			#endif
				
			/*Remove temporary image.*/
				sprintf(cmd,"/bin/rm %stmp_las.img %stmp_las.ddr\n",workDir,workDir);
				execute(cmd);
			}
		
	/*Concatenate frames into a rev. mosaic.*/
		sprintf(cmd,"concat -o %srev_%d",workDir,revs[rev]);
		for (i=0;i<nImages;i++)
			if (revs[rev]==images[i]->rev)
			{
				strcat(cmd," ");
				strcat(cmd,workDir);
				createResName(newName,images[i]);
				strcat(cmd,newName);
			}
		execute(cmd);
		
	/*Blow away individual frames.*/
		for (i=0;i<nImages;i++)
			if (revs[rev]==images[i]->rev)
			{
				createResName(newName,images[i]);
				sprintf(cmd,"/bin/rm %s%s.img %s%s.ddr",workDir,newName,workDir,newName);
				execute(cmd);
			}
	}
	
/*Build the entire mosaic, by concatenating revs.*/
	sprintf(cmd,"concat -o %ssnapshot",workDir);
	for (rev=0;rev<nRevs;rev++)
	{
		strcat(cmd," ");
		sprintf(newName,"%srev_%d",workDir,revs[rev]);
		strcat(cmd,newName);
	}
	/*Add in large coastline.*/
	sprintf(newName," %scoastline_%dm",coastDir,mosaicPixelSize);
	strcat(cmd,newName);
	execute(cmd);
	
	return 0;
}
