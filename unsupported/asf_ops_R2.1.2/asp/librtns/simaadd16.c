/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* add16 (a,b,sum) -----------------------------------------------------
	This routine adds two 16-bit numbers together, and returns
	a 16-bit sum.  The function return value is 1 if there was
	an overflow, or 0 if not.
*/
add16(a,b,sum)
short int a,b;
short int *sum;
{
	int isum;

	isum = a + b;
	*sum = isum & 0xffff;
	return (isum > 32767 || isum < -32768);
}

/* scale16 (a,b,scale,sum) ---------------------------------------------
	This routine adds two 16-bit numbers together, optionally
	divides the result by 2 (scale = 1), and returns the 
	16-bit sum.  The function return value is 1 if there was 
	an overflow, or 0 if not.  If the scaling option is 
	chosen, there can be no overflow.
*/
scale16(a,b,scale,sum)
int a,b,scale;
short int *sum;
{
	int isum;

	isum = a + b;
	if (scale) {
	    *sum = ((isum >> 1) | (isum & 1)) & 0xffff;
	    return (0);
	} else {
	    *sum = isum & 0xffff;
	    return (isum > 32767 || isum < -32768);
	}
}
