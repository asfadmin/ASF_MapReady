/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* itohex(in,h)  ---------------------------------------

        This routine converts an integer value in "in" into an
	ascii hex character string in the character array h.
	NOTE: h must be 9 bytes long, because c will place the
	string terminator character '\0' in the 9th byte.
*/

itohex_(in,h)
	int *in;
	char *h;
{
	sprintf(h,"%.8X",*in);
}
