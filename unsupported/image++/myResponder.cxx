#include "main.h"
#include "imageResp.h"
class myDisplayResponder:public imageDisplayResponder {public:
	
};
image *createDemo(void);
responder * createResponder(int argc,char **argv)
{
	if (argc>=2)
	{
		myDisplayResponder * resp=new myDisplayResponder;
		resp->init3(argc-1,argv,createImage(argv[1]));
		return resp;
	} else {
		printf("Usage: image <las image with extention>\n"
			"This X-Windows program will display any LAS 6.0 image.\n"
			"Also, try \"image -demo\" for a neat visual demo.\n"
			"  ASF STEP Tools, 1997\n");
		exit(1);
	}
	return NULL;
}
