#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		aps2hc_init.c

Description:	This module contains the DCE client initialization routine.
				It includes the procedures performed to obtain binding
				information that will be used to contact the server.

External Functions Defined:
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			

==============================================================================*/
#pragma ident	"@(#)aps2hc_init.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/aps2hc/SCCS/s.aps2hc_init.c"

#include "aps2hc.h"
#include "aps2hc_status.h"


int
aps2hc_init(char *rpc_hostname, handle_t *binding_h, 
				char *progName, unsigned_char_t *prot_seq, int hostFlag )
{
   	unsigned_char_t 	*string_binding;
   	error_status_t		status ;
	int					result ;


   /* Look through hostnames until a binding handle is obtained */
     rpc_string_binding_compose(     /* make string binding from components */
		(unsigned_char_t *)"004efc5a-7b8e-1043-be3b-89e5253caa77", /* UUIDs */ 
		(unsigned_char_t *) prot_seq,   /* protocol sequence */
		(unsigned_char_t *) rpc_hostname,			/* hostname */
		NULL,				/* no endpoint is required */
		NULL,				/* no network options are required */
		&string_binding,		/* the constructed string binding */
		&status);
	result = check_init_status( status, ABORT, progName, hostFlag ) ;

	if (result)
	{
		rpc_binding_from_string_binding(	/*convert string to binding handle*/
							string_binding,	/*input string binding */
							binding_h,		/*binding handle is obtained here*/
							&status);
		result = check_init_status( status, ABORT, progName, hostFlag ) ;
	}

	if (result)
	{
		rpc_string_free(	/* free string binding created */
							&string_binding,
							&status);
		result = check_init_status( status, ABORT, progName, hostFlag ) ;
	}

    return(result);
} /* aps2hc_init */
 
/* End of file */
