/* tfra.c
* test program for fra.c
*/

#include <stdio.h>
#include "/sonja/calval/hans/drdlib/drdapi.h"
#include "andi.h"


main()
{
alInit("a",0L,0L);

while ( luFileOffset = foaGetNextFormat(&myFormat) ){
	px = &(myFormat.s.luImageFormatCount);
	printf("format %8lu\r",GET_SAT_ULONG(myFormat.s.luImageFormatCount));
	/*
	printf("format %8lu at file offset %8lX includes %s\r",
			GET_SAT_ULONG(myFormat.s.luImageFormatCount),
			luFileOffset,
			myFormat.s.bZero ? "ZERO" : "SAR ") ;
	*/
	count++;
	}
printf("\n%ld formats read\n", count);
fraExit();
}
