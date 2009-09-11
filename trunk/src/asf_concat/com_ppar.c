/*******************************************************************************
NAME			       COM_PPAR

PURPOSE	     Compares the projection parameter fields of two input 
	     DDR structures

EXTERNAL ASSOCIATIONS
	This routine is called by SET_PPAR routine

PROGRAM HISTORY
PROGRAMMER		DATE		REASON
----------		----		------
B. Ailts	      Apr. 1988 	original development
B. Ailts	      Aug. 1988		Fixed bug when ddr1.coef[0] == 0
					 and ddr2.coef[0] == 0 - now returns
B. Ailts	      Dec. 1990 	Updated error messages
S. Nelson	      Jan. 1993		Added Hammer and Robinson projections
					to switch statement.  Added variable
					to "sprintf" statement when projection
					invalid.
					
  6.0      6/97    O. Lawlor  Function Prototypes!  Whee!
					
PROJECT       LAS

ALGORITHM 
   Based upon the projection code
      Compare the projection coeficients which are used by the particular 
      projection and set the ppar_flag base upon the comparisions
   return

ALGORITHM REFERENCES	none
*******************************************************************************/

#include "asf.h"

#include "las.h"
#include "proj.h"

#define PREC0 1.		/* precision of 0 places 		*/
#define PREC1 10.		/* precision of 1 place 		*/
#define PREC2 100.		/* precision of 2 places		*/
#define PREC7 10000000.		/* precision of 7 places 		*/
#define PREC9 1000000000.	/* precision of 9 places		*/

double FUNCTION sminor_tbl(int datum);
void FUNCTION comp_sm_esq(struct DDR ddr1,struct DDR ddr2,int *ppar_flag,
	int default_flag,int image1,int image2);
void FUNCTION comp_prec(double ddr1_value,double ddr2_value,int   *ppar_flag,
	int   prec,int   default_flag,int   image1,int   image2);

void FUNCTION com_ppar(struct DDR ddr1,struct DDR ddr2,int *ppar_flag,
	int default_flag,int image1,int image2)
{  /*  com_ppar  */

int j;				/*  loop counter			   */

char errtxt[ERRLEN + 1];	/* temp. error message			   */

/*  Compare only the projection coeficients that have valid values for that
    particular projection.   The precision of the compare is based upon the
    type of value contained within that projection coeficient.
---------------------------------------------------------------------------*/
switch (ddr1.proj_code)
   {
   case GEO:
   case SPCS: 	break;
   
   case UTM:  	if (fabs(floor(ddr1.proj_coef[0] * PREC2) - 
		        floor(ddr2.proj_coef[0] * PREC2)) >= 1)
     	           *ppar_flag = default_flag;
  	      	if (fabs(floor(ddr1.proj_coef[1] * PREC2) - 
		        floor(ddr2.proj_coef[1] * PREC2)) >= 1)
		   {
	    	   *ppar_flag = default_flag;
		   sprintf(errtxt,
		 "Projection parameter values of image %d and %d are not equal",
		   image1,image2);
		   c_errmsg(errtxt,"upddr-equal",NON_FATAL);
		   }
	      	break;

   case ALBERS:
   case LAMCC:	comp_sm_esq(ddr1,ddr2,ppar_flag,default_flag,image1,image2);
		for (j = 2; (j < 8) && ( *ppar_flag != default_flag); j++)
		   comp_prec(ddr1.proj_coef[j],ddr2.proj_coef[j],ppar_flag,
			     PREC2,default_flag,image1,image2);
		break;

   case MERCAT:
   case PS:
   case POLYC:
   case STEREO:	comp_sm_esq(ddr1,ddr2,ppar_flag,default_flag,image1,image2);
		for (j = 4; (j < 8) && ( *ppar_flag != default_flag); j++)
		   comp_prec(ddr1.proj_coef[j],ddr2.proj_coef[j],ppar_flag,
			     PREC2,default_flag,image1,image2);
		break;

   case EQUIDC:	comp_sm_esq(ddr1,ddr2,ppar_flag,default_flag,image1,image2);
		for (j = 2; (j < 8) && ( *ppar_flag != default_flag); j++)
		   comp_prec(ddr1.proj_coef[j],ddr2.proj_coef[j],ppar_flag,
			     PREC2,default_flag,image1,image2);
		comp_prec(ddr1.proj_coef[8],ddr2.proj_coef[8],ppar_flag,
			  PREC0,default_flag,image1,image2);
		break;

   case TM:	comp_sm_esq(ddr1,ddr2,ppar_flag,default_flag,image1,image2);
		comp_prec(ddr1.proj_coef[2],ddr2.proj_coef[2],ppar_flag,
			  PREC7,default_flag,image1,image2);
		for (j = 4; (j < 8) && ( *ppar_flag != default_flag); j++)
		   comp_prec(ddr1.proj_coef[j],ddr2.proj_coef[j],ppar_flag,
			     PREC2,default_flag,image1,image2);
		break;

   case LAMAZ:
   case AZMEQD:
   case GNOMON:
   case ORTHO:
   case EQRECT:
   case VGRINT:	comp_prec(ddr1.proj_coef[0],ddr2.proj_coef[0],ppar_flag,
			  PREC2,default_flag,image1,image2);
		for (j = 4; (j < 8) && ( *ppar_flag != default_flag); j++)
		   comp_prec(ddr1.proj_coef[j],ddr2.proj_coef[j],ppar_flag,
			     PREC2,default_flag,image1,image2);
		break;

   case GVNSP:	comp_prec(ddr1.proj_coef[0],ddr2.proj_coef[0],ppar_flag,
			  PREC2,default_flag,image1,image2);
		comp_prec(ddr1.proj_coef[2],ddr2.proj_coef[2],ppar_flag,
			  PREC1,default_flag,image1,image2);
		for (j = 4; (j < 8) && ( *ppar_flag != default_flag); j++)
		   comp_prec(ddr1.proj_coef[j],ddr2.proj_coef[j],ppar_flag,
			     PREC2,default_flag,image1,image2);
		break;

   case MILLER:
   case HAMMER:
   case ROBIN:
   case MOLL:
   case WAGIV:
   case WAGVII:
   case SNSOID:	comp_prec(ddr1.proj_coef[0],ddr2.proj_coef[0],ppar_flag,
			  PREC2,default_flag,image1,image2);
		comp_prec(ddr1.proj_coef[4],ddr2.proj_coef[4],ppar_flag,
			  PREC2,default_flag,image1,image2);
		comp_prec(ddr1.proj_coef[6],ddr2.proj_coef[6],ppar_flag,
			  PREC2,default_flag,image1,image2);
		comp_prec(ddr1.proj_coef[7],ddr2.proj_coef[7],ppar_flag,
			  PREC2,default_flag,image1,image2);
		break;

   case GOOD:
   case IMOLL:	comp_prec(ddr1.proj_coef[0],ddr2.proj_coef[0],ppar_flag,
			  PREC2,default_flag,image1,image2);
		comp_prec(ddr1.proj_coef[6],ddr2.proj_coef[6],ppar_flag,
			  PREC2,default_flag,image1,image2);
		comp_prec(ddr1.proj_coef[7],ddr2.proj_coef[7],ppar_flag,
			  PREC2,default_flag,image1,image2);
                break;

   case HOM:	comp_sm_esq(ddr1,ddr2,ppar_flag,default_flag,image1,image2);
		comp_prec(ddr1.proj_coef[2],ddr2.proj_coef[2],ppar_flag,
			  PREC7,default_flag,image1,image2);
		for (j = 3; (j < 12) && ( *ppar_flag != default_flag); j++)
		   comp_prec(ddr1.proj_coef[j],ddr2.proj_coef[j],ppar_flag,
			     PREC2,default_flag,image1,image2);
		comp_prec(ddr1.proj_coef[12],ddr2.proj_coef[12],ppar_flag,
			  PREC0,default_flag,image1,image2);
		break;

   case SOM:	comp_sm_esq(ddr1,ddr2,ppar_flag,default_flag,image1,image2);
		for (j = 2; (j < 5) && ( *ppar_flag != default_flag); j++)
		   comp_prec(ddr1.proj_coef[j],ddr2.proj_coef[j],ppar_flag,
			     PREC2,default_flag,image1,image2);
		for (j = 6; (j < 8) && ( *ppar_flag != default_flag); j++)
		   comp_prec(ddr1.proj_coef[j],ddr2.proj_coef[j],ppar_flag,
			     PREC2,default_flag,image1,image2);
		for (j = 8; (j < 10) && ( *ppar_flag != default_flag); j++)
		   comp_prec(ddr1.proj_coef[j],ddr2.proj_coef[j],ppar_flag,
			     PREC7,default_flag,image1,image2);
		comp_prec(ddr1.proj_coef[10],ddr2.proj_coef[10],ppar_flag,
			  PREC0,default_flag,image1,image2);
		comp_prec(ddr1.proj_coef[12],ddr2.proj_coef[12],ppar_flag,
			  PREC0,default_flag,image1,image2);
		break;

   case OBEQA:  comp_prec(ddr1.proj_coef[0],ddr2.proj_coef[0],ppar_flag,
			  PREC2,default_flag,image1,image2);
		for (j = 2; (j < 9) && ( *ppar_flag != default_flag); j++)
		   comp_prec(ddr1.proj_coef[j],ddr2.proj_coef[j],ppar_flag,
			  PREC2,default_flag,image1,image2);
		comp_prec(ddr1.proj_coef[7],ddr2.proj_coef[7],ppar_flag,
			  PREC2,default_flag,image1,image2);
                break;

   default:	sprintf(errtxt,"%d is not a valid GCTP projection",
							ddr1.proj_code);
		c_errmsg(errtxt,"upddr-proj",NON_FATAL);
		c_errmsg(errtxt,"The projection coeficients will be compared with\n\
a precision of 2 decimal places",NON_FATAL);
                c_errmsg(errtxt,"upddr-proj",NON_FATAL);
		for (j= 0; (j < COEFCT) && (*ppar_flag != default_flag); j++)
		   comp_prec(ddr1.proj_coef[j],ddr2.proj_coef[j],ppar_flag,
			     PREC2,default_flag,image1,image2);
		   
   }  /*  switch (ddr1.proj_code)  */
return;
}  /* com_ppar  */

/*****************************************************************************

FUNCTION:	comp_prec

ALGORITHM:
   Compare the two input values according to the input precision
   If not within the precision
      set the ppar_flag to defaul_flag
   return;
  
*******************************************************************************/
void FUNCTION comp_prec(double ddr1_value,double ddr2_value,int   *ppar_flag,
int   prec,int   default_flag,int   image1,int   image2)

{ /*  comp_prec  */



char errtxt[ERRLEN];		/*  buffer for the error message	   */

/*
This is a formula to calculate the coeficient to add to the values
when on the pn9050 because the pn9050 truncates floating point numbers
while the suns and vaxs round.  This truncation may or may not affect the
comparisions.  This is still being examined.
#ifdef gould
double temp1;
double temp2;

int cnt1;
int cnt2;
int gprec;
int i;
int j;
#endif

#ifdef gould
for (i = 1,j = 1; j < COEFCT; j++, i *= 10)
   {
   temp1 = ddr1_value / i;
   temp2 = ddr2_value / i;
   if (temp1 < 1)
      cnt1 = j;
   if (temp2 < 1)
      cnt2 = j;
   }
if (cnt1 > cnt2)
   gprec = COEFCT - cnt1 - prec;
else
   gprec = COEFCT - cnt2 - prec;
if (abs((floor(ddr1_value * prec) + gprec) - 
        (floor(ddr2_value * prec) + gprec)) >= 1)
#else
#endif
*/
if (fabs(floor(ddr1_value * prec) - floor(ddr2_value * prec)) >= 1)
   {
   *ppar_flag = default_flag;
   sprintf(errtxt,
   "Projection parameter values of image %d and %d are not equal",
   image1,image2);
   c_errmsg(errtxt,"upddr-equal",NON_FATAL);
   }

return;
}  /*  comp_prec  */

/*****************************************************************************

FUNCTION:	comp_sm_esq

ALGORITHM:
   If ppar_coef[] of each input ddr is zero
      Return (There is nothing to compare)
   Else
      Based upon the combinations of datum codes and projection coeficients
        (The combinations are based upon the GCTP documentation)
	Calculate the semi major axis and eccentricity squared values
      Compare the calculated semi major axis and eccentricity squared values
   End if

   return;
******************************************************************************/
void FUNCTION comp_sm_esq(struct DDR ddr1,struct DDR ddr2,int *ppar_flag,
	int default_flag,int image1,int image2)

{  /*  comp_sm_esq  */

double eccsq1;			/* first eccentricity squared		    */
double eccsq2;			/* first eccentricity squared		    */
double semi_major1;		/* first semi major axis		    */
double semi_minor1;		/* first semi minor axis		    */
double semi_major2;		/* second semi major axis		    */
double semi_minor2;		/* second semi minor axis		    */


char errtxt[ERRLEN];		/* buffer for the error message		    */

if ((ddr1.proj_coef[0] == 0) && (ddr2.proj_coef[0] == 0))
   return;

else
   {  /* else  */
   if ((ddr1.proj_coef[0] == 0) || (ddr1.datum_code < 0))
      {
      semi_major1 = smajor_tbl(ddr1.datum_code);
      semi_minor1 = sminor_tbl(ddr1.datum_code);
      eccsq1 = ecc_sq(semi_major1,semi_minor1);
      }
   
   else if (ddr1.proj_coef[1] > 1)
      {
      semi_major1 = ddr1.proj_coef[0];
      eccsq1 = ecc_sq(ddr1.proj_coef[0],ddr1.proj_coef[1]);
      }  /*  ddr.proj_coef > 1  */

   else
      {
      semi_major1 = ddr1.proj_coef[0];
      eccsq1 = ddr1.proj_coef[1];
      }  /*  else */

   if ((ddr2.proj_coef[0] == 0) || (ddr2.datum_code < 0))
      {
      semi_major2 = smajor_tbl(ddr2.datum_code);
      semi_minor2 = sminor_tbl(ddr2.datum_code);
      eccsq2 = ecc_sq(semi_major2,semi_minor2);
      }
   
   else if (ddr2.proj_coef[1] > 1)
      {
      semi_major2 = ddr2.proj_coef[0];
      eccsq2 = ecc_sq(ddr2.proj_coef[0],ddr2.proj_coef[1]);
      }  /*  ddr.proj_coef > 1  */

   else
      {
      semi_major2 = ddr2.proj_coef[0];
      eccsq2 = ddr2.proj_coef[1];
      }  /*  else */
   } /* else  */

/*  Compare the semi major axis and eccentricity squared values of each of
    the input DDRs
--------------------------------------------------------------------------*/
if (fabs(floor(semi_major1 * PREC2) - floor(semi_major2 * PREC2)) >= 1)
   {
   *ppar_flag = default_flag;
		   sprintf(errtxt,
		 "Projection parameter values of image %d and %d are not equal",
		   image1,image2);
		   c_errmsg(errtxt,"upddr-equal",NON_FATAL);
		   }

if (fabs(floor(eccsq1 * PREC9) - floor(eccsq2 * PREC9)) >= 1)
   {
   *ppar_flag = default_flag;
   sprintf(errtxt,
   "Projection parameter values of image %d and %d are not equal",
   image1,image2);
   c_errmsg(errtxt,"upddr-equal",NON_FATAL);
   }

return;
}  /*  comp_sm_esq  */

/*****************************************************************************
FUNCTION:	ecc_sq

ALGORITHM:
   Calculate and return the eccentricity squared based upon the semi major
   and semi minor axis
******************************************************************************/
double ecc_sq(double semi_major,double semi_minor);
double ecc_sq(double semi_major,double semi_minor)

{  /*  ecc_sq  */

double eccsq;			/*  eccentricity squared		   */

eccsq = ((semi_major * semi_major) - (semi_minor * semi_minor)) / 
	 (semi_major * semi_major);

return(eccsq);
}  /*  ecc_sq  */

/****************************************************************************

FUNCTION:	smajor_tbl

ALGORITHM:
  create the table of semi major axis to correspond to the datum codes
  return the correct semi major axis
*****************************************************************************/
FUNCTION double smajor_tbl(int datum);
FUNCTION double smajor_tbl(int datum)

{  /*  smajor_tbl  */

double sma_table[20];		/*  semi major axis table		    */

/*  Initailize the table
------------------------*/
sma_table[0] = 6378206.4;
sma_table[1] = 6378249.145;
sma_table[2] = 6377397.155;
sma_table[3] = 6378157.5;
sma_table[4] = 6378388.0;
sma_table[5] = 6378135.0;
sma_table[6] = 6377276.3452;
sma_table[7] = 6378145.0;
sma_table[8] = 6378137.0;
sma_table[9] = 6377563.396;
sma_table[10] = 6377304.063;
sma_table[11] = 6377341.89;
sma_table[12] = 6376896.0;
sma_table[13] = 6378155.0;
sma_table[14] = 6378160.0;
sma_table[15] = 6378245.0;
sma_table[16] = 6378270.0;
sma_table[17] = 6378166.0;
sma_table[18] = 6378150.0;
sma_table[19] = 6370997.0;

return(sma_table[datum]);
}  /*  smajor_tbl  */

/****************************************************************************

FUNCTION:	sminor_tbl

ALGORITHM:
  create the table of semi minor axis to correspond to the datum codes
  return the correct semi minor axis
*****************************************************************************/
double FUNCTION sminor_tbl(int datum)
{  /*  sminor_tbl  */

double smi_table[20];		/*  semi minor table			  */

smi_table[0] = 6356583.8;
smi_table[1] = 6356514.86955;
smi_table[2] = 6356078.96284;
smi_table[3] = 6356772.2;
smi_table[4] = 6356911.94613;
smi_table[5] = 6356750.519915;
smi_table[6] = 6356075.4133;
smi_table[7] = 6356759.769356;
smi_table[8] = 6356752.31414;
smi_table[9] = 6356256.91;
smi_table[10] = 6356103.039;
smi_table[11] = 6356036.143;
smi_table[12] = 6355834.8467;
smi_table[13] = 6356773.3205;
smi_table[14] = 6356774.719;
smi_table[15] = 6356863.0188;
smi_table[16] = 6356794.343479;
smi_table[17] = 6356784.283666;
smi_table[18] = 6356768.337303;
smi_table[19] = 6370997.0;

return(smi_table[datum]);
}  /*  sminor_tbl  */
