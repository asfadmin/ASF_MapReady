#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Function:		print_opln.c

Description:	

int print_opln(
	FILE *fp_in, 
    FILE *fp_out)

    This routine prints out the NASDA OPLN file, which has no line     
  	feeds in it.  The routine adds some formatting for the file header 
  	and the file descriptor, then each data record is printed with a  
    newline character.                                                 
  
Parameters:		
int print_opln(
	FILE *fp_in, 
    FILE *fp_out)

Returns:     	
                 0  no errors.                                         
                -1  unexpected end of file.                            
                -2  error during read.                                 
                -3  file is not an OPLN file.
  
Creator:		Lawrence Stevens

Creation Date:	01/19/1995

Notes:		
==============================================================================*/
#pragma ident	"@(#)print_opln.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/opln_print/SCCS/s.print_opln.c"

/* begin_man_page  */
#include <stdio.h>
#include <string.h>

int print_opln(
	FILE *fp_in, 
    FILE *fp_out)

/*  This routine prints out the NASDA OPLN file, which has no line     */
/*	feeds in it.  The routine adds some formatting for the file header */
/*	and the file descriptor, then each data record is printed with a   */
/*  newline character.                                                 */

/* return code:  0  no errors.                                         */
/*              -1  unexpected end of file.                            */
/*              -2  error during read.                                 */
/*              -3  file is not an OPLN file.                          */
/* end_man_page  */
{
int nbytes;
int j;
char read_rec[300];

/* process the header  */
nbytes = fread(read_rec, 1, 128, fp_in);
read_rec[128] = '\0' ;

/* check for read error.  */
if(nbytes < 0)
	return -2;

/* check for unexpected end of file.  */
if(nbytes == 0)
	return -1;

/* confirm that this file is an OPLN file:  */
if(strncmp(read_rec, "OPLN", 4) != 0 )
	return -3;

fprintf(fp_out, "%80.80s\n%s\n", read_rec, read_rec+80);

/* process the file descriptor.  */
nbytes = fread(read_rec, 1, 278, fp_in);
if(nbytes < 0)
	return -2;
if(nbytes == 0)
	return -1;
read_rec[278] = '\0' ;
fprintf ( fp_out, "file descriptor:  %40.40s\n", read_rec ) ;
for ( j=1; j<15; j++ )
{
	fprintf ( fp_out, "day %2.2d:  %17.17s\n", j, read_rec+(40+(j-1)*17) ) ;
}
fprintf ( fp_out, "\n" ) ;


/* process the data records.  	*/
for (;;) 
{
     /* Read the file to get one data record.     */
	nbytes = fread(read_rec, 1, 80, fp_in);

	if(nbytes < 0)
		return -2;

	if(nbytes == 0)
		break ;

	read_rec[80] = '\0';
	fprintf (fp_out, "%s\n", read_rec ) ;


} /* end for END OF READ LOOP  */

fprintf(fp_out,"END OF FILE\n");

return 0;
}
