/* getrdata(resp_data, resp_addr, resp_length ) -----------------------------
	This routine reads resp_length words of data from the 
	response buffer starting at resp_addr into resp_data array
*/
#include <aspdecl.h>

#define RMODE_MEM_READ 0
#define MEND_EXEC 0xfffff

getrdata( resp_data, resp_addr, resp_length )
short int *resp_data;
int resp_length, resp_addr;
{
	int pgaddr, pgend, i, iend, j, bytes;
	register unsigned short int *m, *r, *rend;
/*
printf("MBAGETRDATA resp_length=%d resp_addr=%x\n", resp_length, resp_addr );
*/
	resp_length <<=2;		/* RESP words to bytes */
	m = (unsigned short int *) resp_data;
	ex_set_resp_mode (RMODE_MEM_READ);
	ex_get_resp_addr( resp_addr, &pgaddr, &i );
	ex_get_resp_addr( resp_addr+resp_length, &pgend, &j );
	if( pgend<pgaddr || (pgend==pgaddr && j<i) ) pgend += 64;
	r = &mb.w[i>>1];	/* set to correct offset in mb page */
	while( pgaddr<=pgend ){
	    iend = (pgaddr==pgend) ? j : MEND_EXEC+1;
	    rend = &mb.w[iend>>1];
	    ex_set_resp_page (pgaddr++);
 	    asp_read( MEM_EXEC, &mb.w[MLOC_EXEC>>1], PAGE_SIZE );
	    bytes = rend - r;
	    bcopy( r, m, bytes*2 );
	    m += bytes;
	    r = &mb.w[MLOC_EXEC>>1];	/* reset to start of mb page */
	}
}
