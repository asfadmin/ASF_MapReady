/* ASF plugin: see static_type at bottom of file for author and purpose */
#include "asf/plugin.h"
#include "asf/image.h"
#include "osl/statistics.h"

/* Plugin name */
#define self plugin_image_checksum

/**
  Information collected about one band of an image.
*/
class bandInfo {
public:
	/** Statistics of pixels: count, min, max, mean, std. dev. */
	osl::statistics::MinMaxSample stat;
	
	enum {nSum=6};
	/** Checksums of various scalings of the pixel values */
	unsigned int checksum[nSum];
	
	bandInfo() {
		for (int i=0;i<nSum;i++) checksum[i]=0u;
	}
	
	/** Add this pixel's value to our information */
	void add(double r) {
		const static double scale[nSum]={
			1.0e-6,1.0e-3,1.0e0,1.0e3,1.0e6,1.0e9
		};
		stat.add(r);
		for (int i=0;i<nSum;i++) checksum[i]+=0x0fFFffFF&(int)(scale[i]*r);
	}
	
	/** Print this band's information */
	std::string print(int bandNo) {
		char buf[1000];
		sprintf(buf,"Band %d: %ld pixels, min %.5g, max %.5g, mean %.5g, stddev %.5g\n"
			"Checksum %d 10^6 mega: 0x%08x\n"
			"Checksum %d 10^3 kilo: 0x%08x\n"
			"Checksum %d 10^0 unit: 0x%08x\n"
			"Checksum %d 10^-3 milli: 0x%08x\n"
			"Checksum %d 10^-6 micro: 0x%08x\n"
			"Checksum %d 10^-9 nano: 0x%08x\n",
			bandNo,stat.size(),stat.get_min(),stat.get_max(),stat.getMean(),stat.getStddev(),
			bandNo,checksum[0],
			bandNo,checksum[1],
			bandNo,checksum[2],
			bandNo,checksum[3],
			bandNo,checksum[4],
			bandNo,checksum[5]
		);
		 
		return buf;
	}
};

class self : public asf::plugin {
	asf::parameter_float_image *src; /**< Input image to checksum */
	bandInfo *bands; /**< Accumulated information about bands of image */
	asf::parameter_string *checksum; /**< Output checksum */
public:
	ASF_plugin_class(self)
	self(asf::plugin_parameters &param) 
		:asf::plugin(param), bands(0)
	{
		asf::input(param,"src",&src);
		asf::output(param,"checksum",&checksum);
	}
	void meta_execute(void) {
		// Reset pixel statistics
		delete[] bands;bands=0;
		bands=new bandInfo[src->bands()];
	}
	void execute(void) { 
		for (int b=0;b<src->bands();b++) {
			ASF_FOR_PIXELS(x,y,src->pixels()) {
				bands[b].add(src->at(x,y,b));
			}
		}
		if ((double)bands[0].stat.size()==src->total_meta_bounds().area())
		{ /* Finally got all the pixels-- write out stats */
			std::string sum="";
			for (int b=0;b<src->bands();b++) sum+=bands[b].print(b);
			*checksum=sum;
			log("%s",sum.c_str());
		}
	}
	~self() {
		delete[] bands;bands=0;
	}
};

/* static */ const asf::type self::static_type(
  /* parent  */ &asf::plugin::static_type,
  /* name    */ "image_checksum",
  /* desc    */ "Compute the checksum of an image's pixels",
  /* authors */ "v1.0 by Orion Lawlor (olawlor@acm.org) 2006/06/06",
  /* version */ 1.0
);
ASF_plugin_def(self)
ASF_plugin_dll(self,image_checksum)
