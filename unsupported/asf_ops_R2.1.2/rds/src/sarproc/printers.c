static char sccsid_printers_c[] =
    "@(#)printers.c	1.2 96/04/09 19:13:31";

#include <stdio.h>
#include "flagerror.h"

/* done by quyen dinh nguyen
 * 12/18/89
 */

/*
 * This subroutine will print out the error message defined in
 * flagerror.h file.
 * INPUT:
    ifd    FILE *   the file description for output messages
                   ( use std or stderr). 
    icode  int*4   the error code defined in flagerror.h file.
 *
 */
printers(ifd,icode)
int  icode;
FILE *ifd;
{
  if(icode == 0)
     fprintf(ifd," no error messages.\n");
  if(icode & OVRFL_PATH)
     fprintf(ifd," ERROR: no path fits with first 100 range cells \n");
  if(icode & OVRFL_A)
     fprintf(ifd," Beware the A value is overflow \n");
  if(icode & OVRFL_2D)
     fprintf(ifd," Beware the TWOD value is overflow  \n");
  if(icode & OVRFL_CRS)
     fprintf(ifd," Beware the coarse value is overflow \n");
  if(icode & OVRFL_FIN)
     fprintf(ifd," Beware the fine value is overflow \n");
}
