/**
  LAS Image File Format "DDR" (Data Descriptor Record) I/O.
  This format comes from the Eros Data Center's Land Analysis System (LAS).
  
  Orion Sky Lawlor, olawlor@acm.org, 2006/05/10 (Copyright ASF)
*/
#include <stdio.h>
#include "ddr.h"

int main(int argc,char *argv[]) {
	DDR d;
	c_getddr(argv[1],&d);
	printf("Read ddr for file %s.  System '%s'.\n"
		"   nl=%d   ns=%d   nbands=%d   type=%d \n"
		"   proj dist (x,y) = %f,%f\n"
		"   upleft (x,y) = %f,%f\n"
		"   loright (x,y) = %f,%f\n",
			argv[1],d.system,
			d.nl,d.ns,d.nbands,d.dtype,
			d.pdist_x,d.pdist_y,
			d.upleft[1],d.upleft[0],
			d.loright[1],d.loright[0]);
	c_putddr("copy.ddr",&d);
	return 0;
}
