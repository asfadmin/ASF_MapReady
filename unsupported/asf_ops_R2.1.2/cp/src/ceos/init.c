/*
    Initialize 
*/
#include "defs.h"
#include "extern.h"

static char sccsid_init_c[] =
        "@(#)init.c	1.4 96/10/01 13:53:19";

/*
    GLOBAL VARIABLE
*/

SARL_ptr *Global_tree;
unsigned char* work;


void init_ceos( void )
{
    extern struct ceos_struct CEOS;
    CEOS.out=FALSE;
    CEOS.debug=0;
    CEOS.inputType=NONO;
    CEOS.odl_out=FALSE;

    /*  get any environment variables */

}
    
