#ifndef DESCFLAG	/*  this is done to avoid multiple defines	*/
struct DESC
{
	unsigned short length;
	unsigned char type;
	unsigned char class;
	char *addr;
};
#define DESCFLAG
#endif

#define CLASS_ARRAY 4
#define TYPE_STRING 14
