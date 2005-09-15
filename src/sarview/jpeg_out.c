/*JPEG Image output routine
Writes the given 2D array of r/g/b components to the given
output file.

Uses the Independent Jpeg Group's LibJPEG

  Orion Lawlor, 5/18/99
*/
#include "main.h"
#include "image.h"

/*jpeglib whines about this*/
#ifdef EXTERN
#  undef EXTERN
#endif
#include "jpeglib.h"
/* global & local function declaration */

void write_jpeg(FILE *fout,int *buf,int wid,int len)
{
	int x,y;
	unsigned char *rgbBuf;
	struct jpeg_error_mgr mgr;
	struct jpeg_compress_struct comp;
	comp.err=jpeg_std_error(&mgr);
	jpeg_create_compress(&comp);
	jpeg_stdio_dest(&comp,fout);

	comp.image_width=wid;
	comp.image_height=len;
	comp.input_components=3;
	comp.in_color_space=JCS_RGB;

	jpeg_set_defaults(&comp);
	jpeg_start_compress(&comp,TRUE);
	rgbBuf=(unsigned char *)MALLOC(sizeof(unsigned char)*3*wid);
	for (y=0;y<len;y++)
	{/*Copy data into the r-g-b format used by the JPEG library*/
		for (x=0;x<wid;x++)
		{
			int bufVal=buf[y*wid+x];
			rgbBuf[x*3+0]=(unsigned char)(bufVal>>16);/*Red component*/
			rgbBuf[x*3+1]=(unsigned char)(bufVal>>8);/*Green component*/
			rgbBuf[x*3+2]=(unsigned char)(bufVal>>0);/*Blue component*/
		}
		jpeg_write_scanlines(&comp,&rgbBuf,1);
	}
	FREE(rgbBuf);
	jpeg_finish_compress(&comp);
	jpeg_destroy_compress(&comp);
}
