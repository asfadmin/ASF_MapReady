#include "main.h"
#include "object.h"
#include "buffer2D.h"
#include "TWZimage.h"

image *createLAS(const char *fileBaseName,const char *ext);
image *createDemoImage(void);
image *createASF(const char *fileBaseName,const char *ext);

#include "object3D.h"
#include "viewer3D.h"

char * strip_extention(char *filename);
image * createImage(const char *imageName)
{
	char *ext,fileBaseName[255];
	strcpy(fileBaseName,imageName);
	ext=strip_extention(fileBaseName);
	if (0==strcmp(ext,"img")||
		0==strcmp(ext,"coh")||
		0==strcmp(ext,"amp")||
		0==strcmp(ext,"phase")||
		0==strcmp(ext,"dem")||
		0==strcmp(ext,"ht"))
	{
		if (0==strcmp(ext,"dem")||
		    0==strcmp(ext,"ht"))
			printf("To get a cool, 3D flythrough of a DEM, try 'image -3D%s'.\n",fileBaseName);
		return createLAS(fileBaseName,ext);
	} else if (0==strncmp(imageName,"-3D",3))
	{
		return create3DViewer(createObject3D(imageName));
	} else if (0==strcmp(ext,"dat")||
		0==strcmp(ext,"D"))
	{
		return createASF(fileBaseName,ext);
	} else if (0==strcmp(imageName,"-demo"))
	{
		return createDemoImage();
	} else {
		printf("Unrecognized extention: '%s' on filename '%s'.\n",ext,imageName);
		return NULL;
	}
}

/* Remove the first extention from a filename.*/
char * strip_extention(char *filename)
{
	char *nullExtention="";
/*We play "Hunt the first extention." by searching for periods starting at the end.*/
	int i=strlen(filename)-1;
	while ((i>0)&&(filename[i]!='.')) 
		i--;
	if (i!=0)
	{
	/*We have an extention, so first we remove it from the filename...*/
		filename[i]=0;
	/*And now we return it.*/
		return &filename[i+1];
	} else return nullExtention;
}
