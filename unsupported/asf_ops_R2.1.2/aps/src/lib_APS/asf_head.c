#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*****************************************************************
*
* Name: asf_head
*
*
* Purpose
*	read in the asf header and check validity
*
* Written by Craig K. Fujimoto
*
* Input
*	fd		i*	input file descriptor
* Output
* Internal
*	comhead	       	c	header buffer
*	intime		c	creation date
*	ftype		c	2 char code for file type
*				NS=NASDA Schedule
*				NP=NASDA Quarterly Request Prelim Assessment
*				ES=ESA Acq Sch
*				AP=APASP
*				AW=AWOSP
*	fdest		c	3 char code for file destination (MPS or APS)
*	fsrc		c	" "    "    "   "    source (ACS)
*	stat		i	return status
*
* 06 Mar 1990 10:11:16	1.0		CRAIG
* $Date$ $Revision$ $Author$
*
*****************************************************************/
#pragma ident	"@(#)asf_head.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_APS/SCCS/s.asf_head.c"

#include "mps.h"
#define ASF_HEAD_LEN 50

int asf_head(int fd, char *filetype)
{
   char comhead[ASF_HEAD_LEN];
   char intime[22],ftype[3],fdest[3],fsrc[3];
   int stat;

   /* Strip off the header from the file */
   /* read the header from the file */
   /* read the \n newline character as well.  this file is from the VAX.  */
   stat = read(fd,&comhead,ASF_HEAD_LEN+1);
   if (stat <= 0) return 1;

   comhead[ASF_HEAD_LEN] = '\0';
/*
   printf("asf_head:  comhead = %s\n", comhead);
 */

/* Parse the header string */
   strncpy(intime,comhead,22);
   strncpy(ftype,&comhead[22],2);
   strncpy(fdest,&comhead[25],3);
   strncpy(fsrc,&comhead[29],3);

/* Check the creation date */
   intime[4] = ':';
   stat = check_date(intime);
   if (stat != 0)
     return 2;

/* Check the file type */
   if (strncmp(ftype,filetype,2) != 0)
     return 2;

/* Check the file destination */
   if (strncmp(fdest,"MPS",3) != 0 && strncmp(fdest,"APS",3) != 0)
     return 2;

/* Check the file source */
   if (strncmp(fsrc,"ACS",3) != 0)
     return 2;

/*
   printf("Header string =XX%sXX\n",comhead);
*/

   return 0;
}
