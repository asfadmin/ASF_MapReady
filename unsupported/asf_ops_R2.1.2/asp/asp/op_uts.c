/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* op_uts.c - Routines used for pre-processing and
   processing.
*/


#include <procfil.h>
#include <procdec.h>
#include <stdio.h>
#include <ctype.h>

#define WOS "wos.pp"       /* Weekly Operations Schelule file name */

extern int asp_hardware_status;	/* PASS = hardware is OK */

/* op_get_diagstat()-----------------------------------
	Returns diagnostic status set by ASP confidence
	tests.
*/

int op_get_diagstat()
{
    return(asp_hardware_status);
}



/* op_connect_DCRS()----------------------------------------
	Commands SARA to route the DCRS to the ASP input.
	This routine may not be necessary.
*/

int op_connect_DCRS()
{
    return(PASS);
}




/* op_answer(q)-----------------------------------------------------
	This routine prints the given question q, and waits for a Y or
	N response.  Any other response causes the question to be
	repeated.
*/

op_answer(q)
	char *q;
{
	int c = 'x';
	int n = 'x';

	while (c != 'N' && c != 'Y') {
	    if (!isspace(c))
		printf("%s ('y' or 'n')? ",q);
	    c = toupper(getchar());
	}
	while ((n = getchar()) != '\n')
	    ;
	if (c == 'Y')
	    return(PASS);
        else return(FAIL);
}




/* op_strstr(str1,str2)------------------------------------------
	This routine finds str2 in str1.
*/

char *op_strstr(str1,str2)
char    *str1, *str2;
{
    char    *strchr();
    char    *s2;          /* Holds pointers that need to be saved */
    static char    *s1;

    s2 = str2;   /* Save original */
    while ((str1 = strchr(str1,*s2)) != NULL)  /* Find first char */
    {
	str2 = s2;   /* Set to original value in case it's changed */
        s1 = str1;              /* Save pointer to first character */
        do
        {
            str1++;
	    str2++;
	    if (*str2 == '\0')    /* End of string to match */
	        return(s1);
        } while (*str1 == *str2);
    }
    return(NULL);
}
