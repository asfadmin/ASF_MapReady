#include "asf.h"
#include "ceos.h"
#include "imageUtil.h"

/***************** CODE *************************
Utility:
	Allocates, extracts, and returns a 
substring of the given string.
*/
char *substr(char *input,int start,int num)
{
	int i;
	char *str=(char *)malloc((num+1)*sizeof(char));
	for (i=0;i<num;i++)
		str[i]=input[start+i];
	str[num]=0;
	return str;
}

/*******************************************
Image:
	Call readImage to fetch the
revolution, frame number, and image version
from an ASF CESO SAR image.
*/
image *readImage(char *name)
{
	char *tmp;
	struct dataset_sum_rec dssr;
	struct VFDRECV facdr;
	image *i=(image *)malloc(sizeof(image));
	strcpy(i->name,name);
	
	get_facdr(i->name,&facdr);
	get_dssr(i->name,&dssr);
	sscanf(tmp=substr(facdr.dataid,2,5),"%d",&(i->rev));free(tmp);
	sscanf(tmp=substr(facdr.imageid,0,3),"%d",&(i->frame));free(tmp);
	sscanf(tmp=substr(facdr.imageid,6,3),"%d",&(i->vers));free(tmp);
	strcpy(i->procvers,facdr.procvers);
	
	i->yaw=facdr.scyaw;
	i->ceosDoppler=dssr.crt_dopcen[0];
	i->prf=dssr.prf1;
	i->dopChange=0.0;
	return i;
}
/***********************************************
dupImage:
	Returns whether or not given image
is a duplicate frame in the given list, and
if it is, eliminates the frame with the smaller
version number.
*/
int dupImage(image *img,image *images[],int nImg)
{
	int i;
	for (i=0;i<nImg;i++)
		if ((images[i]->rev==img->rev)&&
		    (images[i]->frame==img->frame))
		{/*images[i] and img are duplicates.  One must die.*/
			if (images[i]->vers>img->vers)
				printf("Replacing duplicate frame %s with %s\n",img->name,images[i]->name);
			else {
				printf("Replacing duplicate frame %s with %s\n",images[i]->name,img->name);
				images[i]=img;
			}
			return 1;
		}
	return 0;
}
