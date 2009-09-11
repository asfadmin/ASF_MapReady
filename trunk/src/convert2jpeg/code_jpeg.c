#include "asf.h"
#include "ifm.h"
#include "ifm2ppm.h"
#include "jpeglib.h"

void write_image(FILE *fout,Uchar *buf,int wid,int len)
{
	int /*x,*/y;
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
	for (y=0;y<len;y++)
	{
		Uchar *scan[1];
		scan[0]=&buf[y*wid*3];
		jpeg_write_scanlines(&comp,scan,1);
	}
	jpeg_finish_compress(&comp);
	jpeg_destroy_compress(&comp);
}

