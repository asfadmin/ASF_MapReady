/*
Open_dem.c:
	Abstracts the process of opening the DEM image and
extracting the size of the image.
*/

#include "asf.h"
#include "ddr.h"
#include "caplib.h"
#include "dem.h"

/*Open_image: read the image file named "name"
(with extension), and return an image structure
describing it.*/
image *open_image(char *name)
{
	image *im=(image *)malloc(sizeof(image));
	printf("Opening image '%s'\n",name);
	c_getddr(name,&im->ddr);
	extract_proj(&im->ddr,
		&im->pStartX,&im->pStartY,&im->pDistX,&im->pDistY);
	im->ext.v=newSpan(im->ddr.upleft[0],im->ddr.loleft[0]);
	im->ext.h=newSpan(im->ddr.upleft[1],im->ddr.upright[1]);
	strcpy(im->name,name);
	im->f=fopenImage(name,"rb");
	im->readBuf=(float *)malloc(sizeof(float)*im->ddr.ns);
	return im;
}

/*Open_dem: attempts to open height image <name>,
and then error image <name>"_err.img".*/
dem * open_dem(char *name)
{
	char nameBuf[255];
	dem *d=(dem *)malloc(sizeof(dem));
	strcpy(nameBuf,name);
/*Open DEM.*/
	d->height=open_image(nameBuf);
/*Try to open error estimate image.*/
	strtok(nameBuf,".");
	strcat(nameBuf,"_err.img");
	if (fileExists(nameBuf))
		d->err=open_image(nameBuf);
	else
		d->err=NULL;
	return d;
}
