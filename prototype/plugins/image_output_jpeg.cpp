/* ASF plugin: see static_type at bottom of file for author and purpose */
#include "asf/plugin.h"
#include "asf/image.h"
extern "C" {
#include <jpeglib.h> /* Independent JPEG group's jpeg-6b library */
/*@$&_LINK_LIBRARY_REQUIRED: jpeg */
};

/* Plugin name */
#define self plugin_image_output_jpeg
class self : public asf::plugin {
	asf::parameter_filename *filename; /**< Image filename */
	asf::parameter_int *quality; /**< Image compression quality, 0-100 */
	FILE *f; /**< Image file handle */
	struct jpeg_compress_struct cinfo;
	struct jpeg_error_mgr jerr;
	int w,h,bands; /**< Size of output image in pixels */
	int file_y; /**< Row that will be written next to the JPEG file */
	unsigned char *buf; /**< One line of image data */
	
	asf::parameter_float_image *in; /**< Image to write */
public:
	ASF_plugin_class(self)
	self(asf::plugin_parameters &param) 
		:asf::plugin(param), f(0), w(0), h(0),bands(0),buf(0)
	{
		asf::input(param,"filename",&filename);
		asf::input(param,"in",&in,
			asf::parameter_scanline_constraint::instance());
		asf::optional(param,"quality",&quality);
	}
	/** Create the JPEG file and header, and prepare for write. */
	void meta_execute(void) {
		log(20,"meta_execute\n");
		f=fopenImage(filename->c_str(),"wb");
		if (f==NULL) asf::die("Couldn't create file "+(std::string)*filename);
		
		cinfo.err = jpeg_std_error(&jerr);
		jpeg_create_compress(&cinfo);
		jpeg_stdio_dest(&cinfo, f);
		
		w=cinfo.image_width=in->total_meta_bounds().width();
		h=cinfo.image_height=in->total_meta_bounds().height();
		bands=cinfo.input_components=in->bands();
		switch (bands) {
		case 1: /* fine--greyscale. */ 
			cinfo.in_color_space=JCS_GRAYSCALE; 
			break;
		case 3: /* fine--RGB. */
			cinfo.in_color_space=JCS_RGB; 
			break;
		default: /* woa-- can't write that as JPEG! */
			asf::die("JPEG images can only be written as greyscale (1-band) or color (3-band)!");
			break;
		};
		log(10,"creating JPEG image %s of size %d x %d pixels; %d bands\n",filename->c_str(),w,h,bands);
		
		log(20,"set_defaults\n");
		jpeg_set_defaults(&cinfo);
		int qual=95;
		if (quality) qual=*quality;
		jpeg_set_quality(&cinfo,qual,TRUE);
		
		
		log(20,"start_compress: color space=%d\n",cinfo.in_color_space);
		jpeg_start_compress(&cinfo,TRUE);
		buf=new unsigned char[3*w]; /* one scanline of data */
		file_y=0;
	}
	
	/** Write a few more rows from the file */
	void execute(void) { 
		/** We'll read a block of pm.height() rows of our image. */
		asf::pixel_rectangle pm=in->pixel_meta_bounds();
		
		/* Check if we need to seek the file to get to our target */
		log(11,"Write JPEG file (now at line %d) lines %d-%d\n",file_y,pm.lo_y,pm.hi_y-1);
		if (pm.lo_y!=file_y) asf::die("Logic error!  Must write JPEG image in-order, top-down!\n");
		
		asf::pixel_rectangle p=in->pixels();
		
		/** Keep reading rows until we've got all the pixels we need. */
		for (int y=p.lo_y;y<p.hi_y;y++) {
			/* Copy row of JPEG data into our outination image */
			unsigned char *s=buf;
			for (int x=p.lo_x;x<p.hi_x;x++) {
				for (int b=0;b<bands;b++) {
					/* Clip output value and write out */
					float val=in->at(x,y,b);
					unsigned char v;
					if (val<0) v=0;
					else if (val>255) v=255;
					else v=(unsigned char)val;
					*s++=v;
				}
		 	}
			int nWrite=jpeg_write_scanlines(&cinfo,&buf,1);
			if (nWrite!=1) asf::die("Error writing JPEG image "+(std::string)*filename);
			file_y++;
		}
		
		if (file_y==h) { /* done writing--clean up. */
			end_write();
		}
	}
	
	/** Close the JPEG file and finish writing. */
	void end_write(void) {
		if (f) {
			log(20,"finish_compress: %d of %d rows written\n",file_y,h);
			jpeg_finish_compress(&cinfo);
			jpeg_destroy_compress(&cinfo);
			delete[] buf;
			fclose(f);
			f=0;
		}
	}
	
	~self() {
		end_write();
	}
};

/* static */ const asf::type self::static_type(
  /* parent  */ &asf::plugin::static_type,
  /* name    */ "image_output_jpeg",
  /* desc    */ "Read a JPEG image.\n",
  /* authors */ "v1.0 by Orion Lawlor (olawlor@acm.org) 2006/06/06",
  /* version */ 1.0
);
ASF_plugin_def(self)
ASF_plugin_dll(self,image_output_jpeg)
