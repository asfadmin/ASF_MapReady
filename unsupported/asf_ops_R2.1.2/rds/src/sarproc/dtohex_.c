static char sccsid_dtohex__c[] =
    "@(#)dtohex_.c	1.2 96/04/09 19:13:23";

/* dtohex(in,h) ------------------------------------------------

        This routine converts a double-precision variable in "in" 
	into an ascii hex character string in the character array h.
	NOTE: h must be 17 bytes long, because c will place the
	string terminator character '\0' in the 17th byte.
*/

dtohex_(in,h)
	double *in;
	char *h;
{
	sprintf(h,"%.8X%.8X",*in,in[1]);
}
