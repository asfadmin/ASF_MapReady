/*==============================================================================
Filename:	In_Pipe_State.h

Description:	
	This header file contains the data structure used to hold
data regarding the input pipe used between the DCE client and server.

Creator:	Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:		

SCCS Info:
   %W%
==============================================================================*/

#ifndef _INPIPESTATE_
#define _INPIPESTATE_

/*
-- Definition of application-specific state 
-- structure of client pipe data.
*/
typedef struct in_pipe_state 
{ 
   int  filehandle ;       /* handle of client data file */
   char *filename ;        /* name of client data file   */

}  In_Pipe_State ;

#endif /* _INPIPESTATE_ */


/* End of File */
