#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Function:		int quote_doubler
int quote_doubler(
    char *buf_in,        input string perhaps with ' quotes in it.    
    char *buf_out,       output buffer with quotes ' doubled to ''   
    int buf_out_size )   size of buf_out, provided by calling routine 

Description:	
This routine copies the input string from buf_in into buf_out.  If there 
are any single-quote characters, "'", they are doubled to "''".  Note   
that the result string could be up to twice the size of the source       
buffer.  The calling routine must provide buf_out and its size to thie  
routine.                                                                
it is OK to make buf_in and buf_out the same address.                   

buf_out at return is a null-terminated string using at most buf_out_size
bytes.                                                                  

Parameters:		
    char *buf_in,     input string perhaps with ' quotes in it.   
    char *buf_out,    output buffer with quotes ' doubled to ''  
    int buf_out_size )size of buf_out, provided by calling routine

Returns:     	
   Return code:                                                            
               > 0  the length of the destination string in buf_out;        
                    no errors                                             
               = -1 malloc failed.  No memory left.                      
               = -2 buf_out was not big enough for the destination string 
                    buf_out contains the first buf_out_size -1 characters 

Creator:		Lawrence Stevens

Creation Date:	01/27/1995

Notes:		
==============================================================================*/
#pragma ident	"@(#)quote_doubler.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_APS/SCCS/s.quote_doubler.c"

/* begin_man_page  */
#include <stdlib.h>
#include <stdio.h>
int quote_doubler(
    char *buf_in,     /* input string perhaps with ' quotes in it.    */
    char *buf_out,    /* output buffer with quotes ' doubled to ''    */
    int buf_out_size )/* size of buf_out, provided by calling routine */
{

/* This routine copies the input string from buf_in into buf_out.  If there */
/* are any single-quote characters, "'", they are doubled to "''".  Note    */
/* that the result string could be up to twice the size of the source       */
/* buffer.  The calling routine must provide buf_out and its size to thie   */
/* routine.                                                                 */
/* it is OK to make buf_in and buf_out the same address.                    */
/*                                                                          */ 
/* buf_out at return is a null-terminated string using at most buf_out_size */
/* bytes.                                                                   */
/*                                                                          */
/* Return code:                                                             */
/*             > 0  the length of the destination string in buf_out;        */
/*                  no errors                                               */
/*             = -1 malloc failed.  No memory left.                         */
/*             = -2 buf_out was not big enough for the destination string   */
/*                  buf_out contains the first buf_out_size -1 characters   */
/*                                                                          */
/* This routine was created when Sybase command strings used the ' key      */
/* to bound string values, creating a problem with strings with a ' in      */
/* them.  This routine seemed to be a good idea at the time.                */
/* end_man_page  */

char *buf_work;             /* pointer to work area.                   */
int  j;
int  input_str_len;    /* length of input string in buf_in        */
int  n_single_quotes;  /* counter for single quotes encountered.  */

/* initialize  */
buf_work = (char *)malloc(buf_out_size);
if(buf_work == NULL)
{
	fprintf(stderr, "quote_doubler.c:  malloc failed. \n");
		return -1;
}

input_str_len = strlen(buf_in);

/* terminate the buf_work string with a null at the very end.	*/
buf_work[buf_out_size - 1] = '\0';

/*************************************************************************/
/* copy each byte from buf_in to buf_work.  if the byte is a ', then copy     */
/* it again to buf_work.  each byte copied to buf_work is placed just after the    */
/* previous.                                                             */
/* the counter for quotes, n_single_quotes, becomes the difference       */  
/* in the offsets in buf_in and buf_work when a byte is copied.               */
/*************************************************************************/
n_single_quotes = 0;
for ( j = 0; j < input_str_len; j++ )
{ 
	if( (j + n_single_quotes) >= buf_out_size - 1 )
	{
		free(buf_work);
		return -2;
	}
	buf_work[ j + n_single_quotes ] = buf_in[ j ];
	if(buf_in[j] == '\'')
	{
		n_single_quotes++;
		if( (j + n_single_quotes) >= buf_out_size - 1 ) 
		{
			free(buf_work);
			return -2;
		}
		buf_work[ j + n_single_quotes ] = buf_in[ j ];
	}
}

/* terminate the string and copy it into buf_out, the output buffer.	*/
buf_work[input_str_len + n_single_quotes] = '\0';
strcpy(buf_out, buf_work);

free(buf_work);

return (input_str_len + n_single_quotes);

}
