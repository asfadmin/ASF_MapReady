/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* mbaDatToIEEE.c */

#include <aspdecl.h>

/* data_to_ieee(data_in, ieee_out) -------------------------------------
	This routine converts one pair (real, imaginary) of ASP fixed
	point 16-bit data values to 32-bit IEEE standard floating point
	values.
*/
data_to_ieee (data_in, ieee_out)
DATA *data_in;
float ieee_out[2];
{
	ieee_out[0] = data_in->rdat / 32768.0;
	ieee_out[1] = data_in->idat / 32768.0;
}
