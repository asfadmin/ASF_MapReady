/* ASF plugin: see static_type at bottom of file for author and purpose */
#include "asf/plugin.h"
#include "asf/image.h"
#include "asf/ddr.h"

/* Plugin name */
#define self plugin_image_input_las
class self : public asf::plugin {
	asf::parameter_filename *filename; /**< Image filename */
	FILE *f; /**< Image file handle */
	asf::parameter_float_image *out; /**< Image to read */
	DDR ddr; /**< Data descriptor record; for size of image */
	int bytes_band, bytes_row, bytes_pixel;
	std::vector<unsigned char> readbuf; /**< Buffers read image file data */
	
	/** Swap bytes in b for n items of size s bytes each */
	static void swapBytes(unsigned char *b,int n,int s) {
		if (s==1) return; /* byte swapping doesn't affect bytes! */
		for (int i=0;i<n;i++) {
			for (int j=0;j<s;j++)
				std::swap(b[j],b[s-1-j]);
			b+=s;
		}
	}
public:
	ASF_plugin_class(self)
	self(asf::plugin_parameters &param) 
		:asf::plugin(param), f(0)
	{
		asf::input(param,"filename",&filename);
		asf::output(param,"out",&out,asf::parameter_scanline_constraint::instance());
	}
	void meta_execute(void) {
		f=fopenImage(filename->c_str(),"rb");
		if (f==NULL) asf::die("Couldn't read file "+(std::string)*filename);
		c_getddr(appendExt(filename->c_str(),".ddr").c_str(),&ddr);
		switch (ddr.dtype) {
		case DTYPE_BYTE:  bytes_pixel=1; break; 
		case DTYPE_SHORT: bytes_pixel=2; break; 
		case DTYPE_LONG:  bytes_pixel=4; break; 
		case DTYPE_FLOAT: bytes_pixel=4; break; 
		case DTYPE_DOUBLE:bytes_pixel=8; break; 
		default:
			asf::die("Unsupported image datatype in image "+(std::string)*filename);
		};
		bytes_row=bytes_pixel*ddr.ns;
		bytes_band=bytes_row*ddr.nl;
		out->meta_setsize(ddr.nbands,asf::pixel_rectangle(ddr.ns,ddr.nl));
		log(10,"opened image %s of size %d x %d pixels, with %d bands\n",filename->c_str(),ddr.ns,ddr.nl,ddr.nbands);
		log(11,"  bytes_pixel: %d; bytes_row: %d; bytes_band: %d\n",
			bytes_pixel,bytes_row,bytes_band);
	}
	void execute(void) { 
		for (int b=0;b<out->bands();b++) 
		{ /* LAS images are stored in "Band sequential" order on disk,
		   so it's easiest to just read one band at a time. */
			read_band(b);
		}
	}
	void read_band(int b) {
		
		/** We'll read a block of pm.height() rows of our image. */
		asf::pixel_rectangle pm=out->pixel_meta_bounds();
		
		log(11,"  Reading band %d: rows %d to %d  to zoom %d\n",
			b,pm.lo_y,pm.hi_y-1,out->pixel().zoom());
		
		/* Seek to the start of our piece of image data */
		fseek(f,bytes_band*b+bytes_row*pm.lo_y,SEEK_SET);
		
		/* Read image data rows */
		readbuf.resize(bytes_row*pm.height());
		if (1!=fread(&readbuf[0],readbuf.size(),1,f))
			asf::die("I/O error reading image "+(std::string)*filename);
		
		/* Convert image data format during copy to out */
		asf::pixel_rectangle p=out->pixels();
		for (int y=p.lo_y;y<p.hi_y;y++) {
		  unsigned char *row=&readbuf[y*bytes_row+p.lo_x*bytes_pixel];
		  if (ddr.needs_swap) swapBytes(row,(p.hi_x-p.lo_x),bytes_pixel);
		  for (int x=p.lo_x;x<p.hi_x;x++) {
			float v=0.0;
			switch (ddr.dtype) {
			case DTYPE_BYTE:  v=*(unsigned char *)row; break; 
			case DTYPE_SHORT: v=*(short *)row; break; 
			case DTYPE_LONG:  v=*(int *)row; break; 
			case DTYPE_FLOAT: v=*(float *)row; break; 
			case DTYPE_DOUBLE:v=*(double *)row; break; 
			};
			out->at(x,y,b)=v;
			row+=bytes_pixel;
		  }
		}
	}
	
	~self() {
		if (f) fclose(f);
	}
};

/* static */ const asf::type self::static_type(
  /* parent  */ &asf::plugin::static_type,
  /* name    */ "image_input_las",
  /* desc    */ "Read an image from a LAS image file with DDR.\n"
"LAS is the Land Analysis System from the USGS Eros Data Center.\n"
"\n"
"LAS image pixels are stored in a flat binary file, but the image also has\n"
"a separate binary metadata file called the DDR (Data Descriptor Record) \n"
"that gives the image size, data type, and projection coordinates.\n",
  /* authors */ "v1.0 by Orion Lawlor (olawlor@acm.org) 2006/05/11",
  /* version */ 1.0
);
ASF_plugin_def(self)
ASF_plugin_dll(self,image_input_las)
