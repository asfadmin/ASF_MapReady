/******************************************************************************
void:	C_PXSYS

PURPOSE:	Convert data from source computer system format to destination
		computer format.  Data types handled are ECHAR, EBYTE, EWORD,
		EREAL, and EDOUBLE.

PROGRAM HISTORY:
Version    Date     Author       	Change Request
-------    -----    -------------	--------------
  1  	   01/89    K. Gacke		Original development
  2         8/89    K. Gacke		Added ECHAR data type to convert ASCII
					with the high bit on/off.
  3	    8/89    K. Gacke		Fixed bug in vax_r4_conv to check value
					of zero prior to applying exp bias
  4	    2/90    K. Gacke		Fixed bug in align_buf to correctly
					free and assign bufptr.
  5	    8/90    D. Akkerman		Modified to check the insys and outsys
					codes for the possibility of a system
					change between invocations.  This better
					accommodates interactive processing.
  6        10/90    T. Baltzer		check return status's, add errmsg calls
  7	   12/90    K. Gacke		Add cray y-mp conversion routines
  8	    4/91    B. Davis		Added ibm aix.  (Emulate sun-bsd)
  9	    3/91    K. Gacke		Fixed bug in pn_r8_conv routine to
					properly compute the 2's complement
					on a negative number.
  10    6/98    O. Lawlor      Removed dead systems; added little-endian IEEE.

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
	None.

PROJECT			LAS

ALGORITHM 
         
         Floating Point Conversion Algorithm (function pxsys):
             o Supported computer systems:
                     Computer          Referenced in this paper
                     --------          ------------------------
                     DEC VAX/VMS       DEC
                     Gould Power Node  Gould
                     SUN II, III, IV   IEEE
                     IBM AIX           IEEE
                     IBM MVS           IBM
                     PRIME Primos      PRIME 
                     SILICON GRAPHICS  IEEE
		     DATA GENERAL      IEEE
         
             o Convert remote host float number directly into binary 
               format of local host.
         
             o Assume application program will read binary float data 
               created on local or remote host, but writes binary 
               float data only in the local host format, or IEEE 
               standard format.
         
             o Algorithm description to read data created on remote 
               host:  Mask sign bit, exponent field, and fraction 
               field of the float number created on remote host.  Do 
               the floating point arithmetic on the local host to 
               create a valid float number.  If over flow or under 
               flow is detected, conversion routine can optionally 
               assign fill values or return a bad status code.
         
             o Algorithm description to write data into IEEE standard 
               format:  From float number on local host, compute the 
               sign bit, exponent field, and fraction field to make 
               an IEEE float number.  The sign bit is unchanged.  The 
               exponent is computed by taking the natural log of the 
               local host's float number divided by natural log 2, 
               and rounding up to next highest integer (exponent is 
               the smallest integer number such that 2 raised to the 
               power of it is greater than the float number).  The 
               fraction is computed by dividing the float number by 2 
               raised to the power of exponent and multiplying by 
               0x1000000 to create the integer fraction field.
         
             o The conversion algorithms require the use of the power 
               function and the natural logarithm function, both of 
               which are CPU intensive.  To improve the performance, 
               the conversion routines make use of a  array 
               containing values for 2 raised to the power of -126 
               through 127 (IEEE standard).  The exponent field is 
               computed by performing a binary search on the power 
               table.  If the value is outside of the table range, 
               the math functions are used.  The performance was 
               improved by a factor of 2 on the PN9050 and VAX.
         

ALGORITHM REFERENCES	none
*******************************************************************************/
#include "asf.h"



#include "worgen.h"
#include "sysdef.h"
#include "ieee.h"

#define LSH17 (IEEE_pow[17 + IEEE_R4_EXCESS])   /*           0x20000 */
#define LSH21 (IEEE_pow[21 + IEEE_R4_EXCESS])   /*          0x200000 */
#define LSH32 (IEEE_pow[32 + IEEE_R4_EXCESS])   /*       0x100000000 */
#define LSH48 (IEEE_pow[48 + IEEE_R4_EXCESS])   /*   0x1000000000000 */
#define LSH53 (IEEE_pow[53 + IEEE_R4_EXCESS])   /*  0x20000000000000 */
#define LSH56 (IEEE_pow[56 + IEEE_R4_EXCESS])   /* 0x100000000000000 */

void byte2intArr(unsigned char *in,int *out,int nInts);

typedef void (*convFunction)(unsigned char *buf,int size);
typedef struct 
{
	int is_little_endian;
	convFunction r4,r8;
} MACH;

lasErr get_mach(int sys,MACH *system);

lasErr c_pxsys(int insys, unsigned char *buf,int dtype,int size)
    /* (I) input computer system */
   /* unsigned char *buf;  (I/O) array of real numbers to be converted */
   /* int dtype;         (I) data type, EBYTE, EWORD, ELONG, EREAL, EDOUBLE */
   /* int size;          (I) number of elements in array */
{
	static MACH isystem;
	static MACH osystem;
	static int inarch = -1;
	static int outarch = -1;
	lasErr status;
	char errstr[ERRLEN+1];
	unsigned char *bufptr;
	int outsys;
	
	status = E_SUCC;
	bufptr = buf;
	c_sysset(c_getsys(),&outsys);
	if ((inarch != insys) || (outarch != outsys))
	{
		if (get_mach(insys,&isystem) != E_SUCC)
			return(E_FAIL);
		if (get_mach(outsys,&osystem) != E_SUCC)
			return(E_FAIL);
		inarch = insys;
		outarch = outsys;
	}
	switch (dtype)
	{
		case ECHAR:
		case EBYTE:
			break;
		case EWORD:
			if (isystem.is_little_endian ^ osystem.is_little_endian)
				status = c_pxswap(bufptr,size,2);
			break;
		case ELONG:
			if (isystem.is_little_endian ^ osystem.is_little_endian)
				status = c_pxswap(bufptr,size,4);
			break;
		case EREAL:
			if (isystem.is_little_endian ^ osystem.is_little_endian)
				if (E_FAIL== c_pxswap(bufptr,size,4))   return(E_FAIL);
			if (isystem.r4 == osystem.r4)           /* same system */
				break;
			isystem.r4(bufptr,size);
			break;
		case EDOUBLE:
			if (isystem.is_little_endian ^ osystem.is_little_endian)
				if (E_FAIL== c_pxswap(bufptr,size,8)) return(E_FAIL);
			if (isystem.r8 == osystem.r8)           /* same system */
				break;
			/*Convert buffer to native type.*/
			isystem.r8(bufptr,size);
			break;
		default:
			sprintf(errstr,"Invalid data type, dtype = %d",dtype);
			c_errmsg(errstr,"pxsys-badtype",NON_FATAL);
			status = E_FAIL;
			break;
	}
	return status;
}


/*
*  Assign to the MACH structure the computer system's characteristics.
*/
void ieee_r4_conv(unsigned char *buf,int size);
void ieee_r8_conv(unsigned char *buf,int size);
void cray_r4_err(unsigned char *buf,int size);
void cray_r8_conv(unsigned char *buf,int size);
void ibm_r4_conv(unsigned char *buf,int size);
void ibm_r8_conv(unsigned char *buf,int size);

lasErr get_mach(int insys,MACH *isystem)
{
	int status;
	char errstr[ERRLEN+1];

	status = E_SUCC;
	switch (insys)
	{
	case SYS_IEEE_STD:
		isystem->is_little_endian = 0;
		isystem->r4 = ieee_r4_conv;
		isystem->r8 = ieee_r8_conv;
		break;
	case SYS_IEEE_LIL:
		isystem->is_little_endian = 1;
		isystem->r4 = ieee_r4_conv;
		isystem->r8 = ieee_r8_conv;
		break;
	case SYS_IBM_MVS:
		isystem->is_little_endian = 0;
		isystem->r4 = ibm_r4_conv;
		isystem->r8 = ibm_r8_conv;
		break;
	case SYS_CRAY_UNICOS:
		isystem->is_little_endian = 0;
		isystem->r4 = cray_r4_err;
		isystem->r8 = cray_r8_conv;
		break;
	default:
		sprintf(errstr,"Input computer system <%d> not supported",
		        insys);
		c_errmsg(errstr,"pxsys-badinsys",NON_FATAL);
		status = E_FAIL;
		break;
	}
	return(status);
}

void cray_r4_err(unsigned char *buf,int size)
{
	c_errmsg("ERROR! Cray had no 32-bit type!\n","pxsys",LAS_FATAL);
}
/*
*  This routine converts an array of eight byte Cray float numbers
*  into an array of float numbers of the host computer.
*
*  The internal representation of the Cray 8-byte float number is:
*      63            48 47                    0
*       ---------------------------------------
*       |S|S|exponent  |   fraction           |
*       ---------------------------------------
*
*  Bit(s)	Description
*  ------	-----------
*   63		Sign bit of the fraction. 1=>negative, 0=>positive
*   62		Sign bit of the exponent. 1=>positive, 0=>negative
*  48-61	exponent is a power of 2
*   0-47	fraction
*
*/
void cray_r8_conv(unsigned char *buf,int size)
{

double frac;
double *newval;
int cur;

int sign;
int  exp;
unsigned int  fmask1;
unsigned int  fmask2;
unsigned int  num[2];
unsigned char *curptr;


curptr = buf;
newval = (double *) buf;
for (cur = 0; cur < size; cur++,curptr+=8,newval++)
    {
    byte2intArr(curptr,(int *)num,2);
    sign = (num[0] & 0x80000000) ? -1 : 1;
    exp = (num[0] & 0x3fff0000) >> 16;
    if (!(num[0] & 0x40000000)) 		/* check exp sign bit 0=>negative */
        exp -= 0x4000;
    fmask1 = (num[0] & 0x0000ffff);
    fmask2 = (num[1] & 0xffffffff);
    if ((num[0] == 0) && (num[1] == 0))	/* value = 0 */
        {
        *newval = 0;
        continue;
        }
    frac = ((fmask1 * (double) LSH32) + fmask2) / (double) LSH48;
    if ((IEEE_MINEXP <= exp) && (exp <= IEEE_MAXEXP))
        *newval = frac * IEEE_pow[exp+IEEE_R4_EXCESS] * sign;
    else
        *newval = frac * pow((double) 2.0,(double) exp) * sign;
    }
}

/*
*  This routine converts an array of four byte IBM MVS float numbers
*  into an array of float numbers of the host computer.
*
*  The internal representation of the IBM MVS 4-byte float number is:
*       0  1           7 8                    31
*       ----------------------------------------
*       |S |  exponent  |   fraction           |
*       ----------------------------------------
*  Sign bit is a 1 if the number is negative
*  Exponent is a 7-bit binary number with a bias of 64 to which the
*       base 16 is raised
*  Fraction is a 24-bit hexadecimal normalized number with a radix point to
*       the left of the highest order fraction bit (bit 8)
*
*  Algorithm consists of masking the sign bit (bit 0), exponent field
*  (bits 1 - 7), and fraction field (bits 8 - 31).  After obtaining the 
*  exponent and fraction the floating point arithmetic of the host 
*  computer is used to compute the float number.
*/

void  ibm_r4_conv(unsigned char *buf,int size)
{

float frac;
float *newval;
int cur;
int sign;
int  exp;
unsigned int  ival;
unsigned int  fmask;


newval = (float *) buf;
for (cur = 0; cur < size; cur++,++newval)
    {
    byte2intArr(&buf[cur*4],(int *)&ival,1);
    sign = (ival & 0x80000000) ? -1 : 1;
    exp = ((ival & 0x7f000000) >> 24);
    fmask = ival & 0x00ffffff;
    if ((exp == 0) && (fmask == 0))
        {
        *newval = 0.0;
        continue;
        }
    exp = (exp - 64) * 4;               /* convert from base 16 to base 2 */
    frac = (float) fmask / 0x1000000;
    if ((IEEE_MINEXP <= exp) && (exp <= IEEE_MAXEXP))
        *newval = frac * IEEE_pow[exp+IEEE_R4_EXCESS] * sign;
    else
        *newval = frac * (float) pow((double) 2.0,(double) exp) * sign;
    }
}

/*
*  This routine converts an array of eight byte IBM MVS double numbers
*  into an array of double numbers of the host computer.
*
*  The internal representation of the IBM MVS 8-byte float number is:
*       0 1           7 8                    63
*       ---------------------------------------
*       |S|  exponent  |   fraction           |
*       ---------------------------------------
*  Sign bit is a 1 if the number is negative
*  Exponent is a 7-bit binary number with a bias of 64 to which the
*       base 16 is raised
*  Fraction is a 56-bit hexadecimal normalized number with a radix point to
*       the left of the highest order fraction bit (bit 8)
*
*  Algorithm consists of masking the sign bit (bit 0), exponent field
*  (bits 1 - 7), and fraction field (bits 8 - 63).  After obtaining the 
*  exponent and fraction the floating point arithmetic of the host 
*  computer is used to compute the float number.
*/

void  ibm_r8_conv(unsigned char *buf,int size)
{

double frac;
double *newval;
int cur;
int sign;
int  exp;
unsigned int fmask1;
unsigned int fmask2;
unsigned int num[2];
unsigned char *curptr;


curptr = buf;
newval = (double *) buf;
for (cur = 0; cur < size; cur++,curptr+=8,newval++)
    {
    byte2intArr(curptr,(int *)num,2);
    sign = (num[0] & 0x80000000) ? -1 : 1;
    exp = ((num[0] & 0x7f000000) >> 24);
    fmask1 = num[0] & 0x00ffffff;
    fmask2 = num[1] & 0xffffffff;
    if ((exp == 0) && (fmask1 == 0) && (fmask2 == 0))
        {
        *newval = 0.0;
        continue;
        }
    exp = (exp - 64) * 4;               /* convert from base 16 to base 2 */
    frac = (double) ((fmask1 * (double) LSH32) + fmask2) / (double) LSH56;
    if ((IEEE_MINEXP <= exp) && (exp <= IEEE_MAXEXP))
        *newval = frac * IEEE_pow[exp+IEEE_R4_EXCESS] * sign;
    else
        *newval = frac * pow((double) 2.0,(double) exp) * sign;
    }
}

/*
*  This routine converts an array of four byte IEEE float numbers
*  into an array of float numbers of the host computer.
*
*  The internal representation of the IEEE 4-byte float number is:
*      31 30         23 22                    0
*       ---------------------------------------
*       |S|  exponent  |   fraction           |
*       ---------------------------------------
*  Sign bit is a 1 if the number is negative
*  Exponent is a 8-bit real number with an excess of 127 to which the
*       base 2 is raised
*  Fraction is a 23-bit hexadecimal normalized number with an implicit
*       1 in bit 24
*
*  Algorithm consists of masking the sign bit (bit 31), exponent field
*  (bits 30 - 23), and fraction field (bits 22 - 0).  After obtaining the
*  exponent and fraction, the floating point arithmetic of the host computer
*  is used to compute the float number.
*/

void  ieee_r4_conv(unsigned char *buf,int size)
{
float frac;
float *outval;
int  cur;
int  exp;
int  sign;
unsigned int fmask;
unsigned int ival;

outval = (float *) buf;

for (cur=0; cur<size; cur++)
    {
    byte2intArr(&buf[cur*4],(int *)&ival,1);
    sign = (ival & 0x80000000) ? -1 : 1;
    exp = ((ival & 0x7f800000) >> 23);
    fmask = (ival & 0x007fffff);
    if (exp == 0)		/* value = 0, normal and denormalized */
        {
        outval[cur] = 0.0;
        continue;
        }
    fmask |= 0x00800000;
    frac = (float) fmask / 0x1000000;
    outval[cur] = frac * (float) IEEE_pow[exp] * sign;
    }
}

/*
*  This routine converts an array of eight byte IEEE standard double numbers
*  into an array of double numbers of the host computer.
*
*  The internal representation of the IEEE standard 8-byte double number is:
*      63 62         52 51                    0
*       ---------------------------------------
*       |S|  exponent  |   fraction           |
*       ---------------------------------------
*  Sign bit is a 1 if the number is negative
*  Exponent is a 11-bit real number with an excess of 1023 to which the
*       base 2 is raised
*  Fraction is a 52-bit hexadecimal normalized number with an implicit
*       1 in bit 53
*
*  Algorithm consists of masking the sign bit (bit 63), exponent field
*  (bits 62 - 52), and fraction field (bits 51 - 0).  After obtaining the
*  exponent and fraction, the floating point arithmetic of the host computer
*  is used to compute the float number.
*/

void ieee_r8_conv(unsigned char *buf,int size)
{
double frac;
double *newval;
int cur;
int sign;
int  exp;
unsigned int  fmask1;
unsigned int  fmask2;
unsigned int  num[2];
unsigned char *curptr;

curptr = buf;
newval = (double *) buf;
for (cur = 0; cur < size; cur++,curptr+=8,newval++)
    {
    byte2intArr(curptr,(int *)num,2);
    sign = (num[0] & 0x80000000) ? -1 : 1;
    exp = (num[0] & 0x7ff00000) >> 20;
    fmask1 = (num[0] & 0x000fffff);
    fmask2 = (num[1] & 0xffffffff);
    if (exp == 0) 		/* value = 0, normal and denormalized */
        {
        *newval = 0;
        continue;
        }
    exp -= 1022;      
    fmask1 |= 0x00100000;
    frac = ((fmask1 * (double) LSH32) + fmask2) / (double) LSH53;
    if ((IEEE_MINEXP <= exp) && (exp <= IEEE_MAXEXP))
        *newval = frac * IEEE_pow[exp+IEEE_R4_EXCESS] * sign;
    else
        *newval = frac * pow((double) 2.0,(double) exp) * sign;
    }
}

