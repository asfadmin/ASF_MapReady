/* Config.h:
	This include file specifies several configurable parameters
for the write_dlt program.*/

/*#defines:  This must be set according to what your DLT is called.*/
#define testing 0
#if testing
  #define DLT_DEVICE "/dev/null"
#else
  #define DLT_DEVICE "/dev/rmt/0n"
#endif
#if testing
  #define TAPE_DEVICE "/dev/null"
#else
  #define TAPE_DEVICE "/dev/rmt/1n"
#endif

/*An e-mail listing of the freshly-written tape's contents is
e-mailed to these people:*/
#define EMAIL_RECIPIENTS "mmason@gi.alaska.edu \
		rguritz@images.alaska.edu \
		olawlor@images.alaska.edu"
