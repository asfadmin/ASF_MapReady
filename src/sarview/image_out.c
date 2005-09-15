/*SARview image_out.c:
The image output routines.
Converts the currently selected portion of the current image into a
byte array, and saves it as a JPEG-format graphic image.
*/
#include "main.h"
#include "image.h"

void write_jpeg(FILE *fout,int *buf,int wid,int len);

int image_saveJpeg(const char * imageName)/* TCL asks C to 
	save the current image to the given filename.  Returns 0 on failure; 1 on sucess.*/
{
	char *fName=appendExt(imageName,".jpg");
	FILE *fout;
	int x,y,color;
	int wid,len;
	selection *s=selBounds(&wid,&len);
	float *inBuf=(float *)MALLOC(sizeof(float)*wid);
	float mapsToZero=(float)(-image.offset/image.slope);
	int *buf=(int *)MALLOC(sizeof(int)*wid*len);/*Output image pixels*/
	for (y=0;y<len;y++)
		for (x=0;x<wid;x++)
			buf[y*wid+x]=0;/*Zero out buffer before we draw*/
	for (color=0;color<ddr->nbands;color++)
	{
		for (y=0;y<len;y++)
		{
			selLine(s,color,y,inBuf,mapsToZero);
			/*Now that we have the data to write out as floats,
			render it to our byte array*/
			if (ddr->nbands==1)
				drawFloatsToGrey(inBuf,&buf[y*wid],wid);
			else
				drawFloatsToColor(inBuf,&buf[y*wid],wid,color);
		}
	}
	FREE(inBuf);
	deleteSel(s);

/*Now that we've constructed our image as a byte array, write it as a JPEG*/
	fout=FOPEN(fName,"wb");
	write_jpeg(fout,buf,wid,len);
	FCLOSE(fout);
	FREE(buf);
	return 1;
}
