#ifndef _ASF_ENDIAN_H_
#define _ASF_ENDIAN_H_

/*Endian.h:
	An include file to deal with byte-ordering
issues.  Contains definitions for byte-swap functions
in asf_meta.a.*/

/*These routines swap high and low bytes.
This converts big-endian numbers to little-endian,
AND vice versa-- because swapping is symmetric.*/

void swap16(unsigned char *in);
void swap32(unsigned char *in);
void swap64(unsigned char *in);

/*These routines convert 16, 32, or 64 bit data
to/from big or little endian format.

Thus, for example, if you've read in a 16-bit big-endian 
integer "x" from a file, call big16(x).  On a big-endian
machine, this does nothing; but on a little-endian
machine, this swaps the bytes properly.  Before writing
out big-endian data, do the same! */
#if defined(big_endian)
#define big16(x) 
#define big32(x) 
#define big64(x) 
#define lil16(x) swap16((unsigned char *)&(x))
#define lil32(x) swap32((unsigned char *)&(x))
#define lil64(x) swap64((unsigned char *)&(x))

#elif defined(lil_endian)
#define big16(x) swap16((unsigned char *)&(x))
#define big32(x) swap32((unsigned char *)&(x))
#define big64(x) swap64((unsigned char *)&(x))
#define lil16(x) 
#define lil32(x) 
#define lil64(x) 
#else
#error "You must set either big_endian or lil_endian"
#endif

/*These routines interpret the bytes in an array as integers:*/
int lilInt16(unsigned char *in);
int lilInt32(unsigned char *in);
int bigInt16(unsigned char *in);
int bigInt32(unsigned char *in);
/*These routines write integers into bytes in an array:*/
void lilInt16_out(int in,unsigned char *out);
void lilInt32_out(int in,unsigned char *out);
void bigInt16_out(int in,unsigned char *out);
void bigInt32_out(int in,unsigned char *out);

/*These routines convert 16, 32, or 64 bit floating-point data
to/from big or little endian format.*/
#if defined(big_ieee)
#define ieee_big32(x) 
#define ieee_big64(x) 
#define ieee_lil32(x) swap32((unsigned char *)&(x))
#define ieee_lil64(x) swap64((unsigned char *)&(x))

#elif defined(lil_ieee)
#define ieee_big32(x) swap32((unsigned char *)&(x))
#define ieee_big64(x) swap64((unsigned char *)&(x))
#define ieee_lil32(x) 
#define ieee_lil64(x) 
#else
#error "You must set either big_ieee or lil_ieee"
#endif


#endif
