#ifndef _LAS_H_
#define _LAS_H_

#include "worgen.h"
#include "ddr.h"        /* data descriptor record include file               */
#include "display.h"    /* include file to display messages to terminal      */
#include "imageio.h"	/* image I/O include file                            */
#include "cm.h"		/* file naming include file                          */

void FUNCTION c_calcor(const double *sl,const double *ss,const double *nl,const double *ns,
	const struct DDR *ddr, double *upleft,double *upright,double *loleft,double *loright);
lasErr FUNCTION c_comsys(char *hostname,int *flag);
void FUNCTION c_lsmknm(const char *inname,const char *ext,char *outname);
lasErr FUNCTION c_gettyp(const char *hosin[],int   *nmrimg,int   *odtype);

lasErr FUNCTION c_lused(const char *hostname);

void byte2intArr(unsigned char *inBuf,int *outArr,int nInts);
void int2byteArr(int *inArr,unsigned char *outBuf,int nInts);

#endif
