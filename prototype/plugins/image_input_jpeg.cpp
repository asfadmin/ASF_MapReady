/* ASF plugin: see static_type at bottom of file for author and purpose */
#include "asf/plugin.h"
#include "asf/image.h"
extern "C" {
#include <jpeglib.h> /* Independent JPEG group's jpeg-6b library */
/*@$&_LINK_LIBRARY_REQUIRED: jpeg */
};

/* Plugin name */
#define self plugin_image_input_jpeg
class self : public asf::plugin {
	asf::parameter_filename *filename; /**< Image filename */
	FILE *f; /**< Image file handle */
	struct jpeg_decompress_struct cinfo;
	struct jpeg_error_mgr jerr;
	int w,h,bands; /**< Size of output image in pixels */
	int file_y; /**< Row that will be read next from the JPEG file */
	unsigned char *buf; /**< One line of image data */
	
	asf::parameter_float_image *out; /**< Image to read */
public:
	ASF_plugin_class(self)
	self(asf::plugin_parameters &param) 
		:asf::plugin(param), f(0), w(0), h(0),bands(0),buf(0)
	{
		asf::input(param,"filename",&filename);
		asf::output(param,"out",&out,asf::parameter_scanline_constraint::instance());
	}
	void meta_execute(void) {
		begin_read();
	}
	
	/** Open the JPEG file, parse the header, and prepare for read. */
	void begin_read(void) {
		f=fopenImage(filename->c_str(),"rb");
		if (f==NULL) asf::die("Couldn't read file "+(std::string)*filename);
		
		cinfo.err = jpeg_std_error(&jerr);
		jpeg_create_decompress(&cinfo);
		jpeg_stdio_src(&cinfo, f);
		jpeg_read_header(&cinfo, TRUE);
		
		/** We now know the size of the input image.
		   We *always* read 3-band (RGB) images.
		*/
		w=cinfo.image_width, h=cinfo.image_height;
		bands=cinfo.num_components;
		out->meta_setsize(bands,asf::pixel_rectangle(w,h));
		
		jpeg_start_decompress(&cinfo);
		buf=new unsigned char[3*w]; /* one scanline of data */
		file_y=0;
		log(10,"opened JPEG image %s of size %d x %d pixels; %d bands\n",filename->c_str(),w,h,bands);
	}
	
	/** Read a few more rows from the file */
	void execute(void) { 
		/** We'll read a block of pm.height() rows of our image. */
		asf::pixel_rectangle pm=out->pixel_meta_bounds();
		
		/* Check if we need to seek the file to get to our target */
		log(11,"Reading JPEG file (now at line %d) lines %d-%d\n",file_y,pm.lo_y,pm.hi_y-1);
		if (pm.lo_y<file_y) { 
			log(10,"Reopening JPEG file to fake a backward seek from line %d to line %d\n",file_y,pm.lo_y);
			end_read(); begin_read();
		}
		if (pm.lo_y>file_y) {
			log(10,"Skipping over JPEG data to fake a forward seek from line %d to line %d\n",file_y,pm.lo_y);
			for (;file_y<pm.lo_y;file_y++) jpeg_read_scanlines(&cinfo,&buf,1);
		}
		
		asf::pixel_rectangle p=out->pixels();
		if (!(file_y==pm.lo_y)) asf::die("Logic error in JPEG line maintainance!");
		if (!(p.lo_x==0 && p.hi_x==w)) asf::die("Logic error in JPEG row size!");
		
		/** Keep reading rows until we've got all the pixels we need. */
		for (int y=p.lo_y;y<p.hi_y;y++) {
			int nRead=jpeg_read_scanlines(&cinfo,&buf,1);
			if (nRead!=1) asf::die("Error reading JPEG image "+(std::string)*filename);
			file_y++;
			/* Copy row of JPEG data into our outination image */
			unsigned char *s=buf;
			for (int x=p.lo_x;x<p.hi_x;x++) {
				for (int b=0;b<bands;b++) {
					out->at(x,y,b)=*s++; 
				}
		 	}
		}
	}
	
	/** Close the JPEG file and finish reading. */
	void end_read(void) {
		if (f) {
			jpeg_destroy_decompress(&cinfo);
			delete[] buf;
			fclose(f);
			f=0;
		}
	}
	
	~self() {
		end_read();
	}
};

/* static */ const asf::type self::static_type(
  /* parent  */ &asf::plugin::static_type,
  /* name    */ "image_input_jpeg",
  /* desc    */ "Read a JPEG image.\n",
  /* authors */ "v1.0 by Orion Lawlor (olawlor@acm.org) 2006/06/06",
  /* version */ 1.0
);
ASF_plugin_def(self)
ASF_plugin_dll(self,image_input_jpeg)
