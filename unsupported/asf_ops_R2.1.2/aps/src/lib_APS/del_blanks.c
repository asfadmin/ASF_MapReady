#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/********************************************************************
*                                                                   
*  Name: del_blanks
*                                                                   
*  Purpose: delete blanks from the beginning and end of a character string
*          
*
*  Input Parameters:
*	Name	Type    Definition
*	str	c*	pointer to string
*
*  Output Parameters:
*	str	c*	string  w/o blanks
*
*  Locals :
*	str2	c*	string place holder
*                                                                   
*  $Date$ $Revision$ $Author$
*********************************************************************/
#pragma ident	"@(#)del_blanks.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_APS/SCCS/s.del_blanks.c"

void del_blanks(char	*str)
{
char *str2;

  /* get the pointer to the first non-blank:  */
  for ( str2 = str; *str2 == ' ' ; str2++ ) ;

  /* shift the string over.  */
  strcpy(str,str2);

  /* get the pointer to the first null - limit of 90 chars	*/
  for ( str2 = str; (*str2 != '\0') && (str2 - str < 90) ; str2++ ) ;

  if ( (str2 == str) || (str2 - str >= 90) ) return ;

  /* get the pointer to the last non-blank in the string by looking 	*/
  /* backwards starting at the character before the null 		*/
  for ( str2 -- ; (*str2 == ' ') && (str2 >= str) ; str2-- ) ;

  /* set the pointer to the position after the first non-blank	*/
  str2++ ;

  /* terminate the string just after the last non-blank		*/
  *str2 = '\0' ;
  
}
