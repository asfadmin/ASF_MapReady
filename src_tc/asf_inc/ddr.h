/*This is the 'C' include file for the DDR routines			     */
#ifndef __DDR_H

#define __DDR_H   /* include only once */
#include "asf.h"

struct DDR                /* ddr for integer data */
    {
    int nl;              /* number of lines in image                */
    int ns;              /* number of samples in image              */
    int nbands;          /* number of bands in image                */
    int dtype;           /* data type of pixels:                    */
                          /*   =1:  unsigned char                    */
			  /*   =2:  short                            */
                          /*   =3:  int                             */
                          /*   =4:  float                            */
    int master_line;     /* line relative to master image	     */
    int master_sample;   /* sample relative to master image         */
    int valid[8];	  /* valid flags:                            */
                          /*   =0:  not valid                        */
                          /*   =1:  valid                            */
 		      	  /*   =2:  unknown		             */
    int proj_code;       /* GCTP projection code                    */
                          /*  refer to GCTP document for valid codes */
    int zone_code;       /* UTM or State Plane zone                 */
                          /*  refer to GCTP document for valid codes */
    int datum_code;      /* GCTP datum code                         */
                          /*  refer to GCTP document for valid codes */
    int spare;		  /* spare integer value; for future use or
			     expansion				     */

    char system[12];       /* computer system data is on (with NULL)  */
    char proj_units[12];   /* Projection units (GCTP units+other) (with NULL) */
    char last_used_date[12]; /* last access date	(with NULL)	     */
			    /* NOTE: All ddr dates are stored as     */
			    /* "dd-mmm-yy".  For example, a date     */
			    /* of December 31, 1986 is stored as     */
			    /* "31-dec-86" with the month in lower   */
			    /* case.				     */
    char last_used_time[12]; /* last access time	(with NULL)  */
			    /* NOTE: All ddr times are stored        */
			    /* using a twenty-four hour clock.	     */
			    /* Seconds are speperated by a colon.    */
			    /* Example: 1:05:55 pm is stored as      */
			    /* 1305:55				     */

    double proj_coef[15]; /* GCTP projection parameters              */
                          /*  refer to GCTP document for field definitions*/
    double upleft[2];     /* Corner coordinates entered              */
    double loleft[2];     /*   in latitude/longitude or              */
    double upright[2];    /*   northing/easting order or             */
    double loright[2];    /*   projection coordinates (y/x)          */
    double pdist_y;       /* projection distance/pixel (y-direction) */
    double pdist_x;       /* projection distance/pixel (x-direction) */
    double line_inc;      /* line increment for sampling             */
    double sample_inc;    /* sample increment for sampling           */
                          /* NOTE:  The line/sample increments are   */
                          /*   the values applied to the original    */
                          /*   image to obtain this image.  If re-   */
                          /*   or sub-sampling was not applied, the  */
                          /*   value contained in these fields will  */
                          /*   be 1.0                                */
    };

struct BDDR
    {
    int bandno;	  /* band no. of the record 		     */
    int valid;		  /* min/max validity flag of the band	     */
                          /*   =0:  not valid                        */
                          /*   =1:  valid                            */
 		      	  /*   =2:  bounded		             */

    double minval;	  /* minimum value of the band		     */
    double maxval;	  /* maximum value of the band		     */

    char source[32];	  /* source of data (with NULL)		     */
    char instrument[32];  /* type of sensor (with NULL)		     */
    char direction[64];	  /* direction of capture process(with NULL) */
    char date[10];	  /* capture date (with NULL)		     */
			  /* NOTE: All ddr dates are stored as       */
			  /* "dd-mmm-yy".  For example, a date       */
			  /* of December 31, 1986 is stored as	     */
			  /* "31-dec-86" with the month in lower     */
			  /* case.				     */
    char time[8];	  /* capture time (with NULL)		     */
			  /* NOTE: All ddr times are stored          */
			  /* using a twenty-four hour clock.	     */
			  /* Seconds are speperated by a colon.	     */
			  /* Example: 1:05:55 pm is stored as        */
			  /* 1305:55				     */
    };

/*  Below are the constants to be used as keys within GETDDF and PUTDDF
-----------------------------------------------------------------------*/
#define DDNL 0		  /* number of lines			       */
#define DDNS 1		  /* number of samples			       */
#define DDNB 2		  /* number of bands			       */
#define DDDTYP 3	  /* Data type               	 	       */
#define DDML 4		  /* Master line    			       */
#define DDMS 5		  /* Master sample 		               */
#define DDVFLG 6	  /* Validity flag array     		       */
#define DDPCOD 7	  /* Projection code			       */
#define DDZCOD 8	  /* Zone code      			       */
#define DDDCOD 9	  /* Datum code  		     	       */
#define DDSYS 10	  /* System      		               */
#define DDUNIT 11	  /* Projection Unit		               */
#define DDLDAT 12	  /* Last use dated   		               */
#define DDLTIM 13	  /* Last used time		               */
#define DDPCF 14	  /* Projection coeficients		       */
#define DDUL 15		  /* Upper left     		               */
#define DDLL 16		  /* Lower left       		               */
#define DDUR 17		  /* Upper right       		               */
#define DDLR 18		  /* Lower right            		       */
#define DDPDY 19	  /* Projection distance-y  		       */
#define DDPDX 20	  /* Projection distance-x   		       */
#define DDLINC 21	  /* Line increment          		       */
#define DDSINC 22	  /* Sample increment        		       */

/*  Below are the constants to be used as keys within GETBDF and PUTBDF
-----------------------------------------------------------------------*/
#define DDBAND 100	  /* band number    			       */
#define DDMMV 101	  /* Min/max validity flag 		       */
#define DDMIN 102	  /* Minimum value    	      		       */
#define DDMAX 103	  /* Maximum value    	      		       */
#define DDSRC 104	  /* Source           	      		       */
#define DDINST 105	  /* Instrument      		     	       */
#define DDDIR 106	  /* Direction       		      	       */
#define DDCDAT 107	  /* Capture date    		      	       */
#define DDCTIM 108	  /* Capture time    			       */

/* Below are the constants to retrieve the correct validity flag from the
   validity flag array						         
-----------------------------------------------------------------------*/
#define DDPCV 0	  	/* projection code validity flag	       */
#define DDZCV 1	  	/* zone code validity flag		       */
#define DDDCV 2	  	/* datum code validity flag		       */
#define DDPPV 3	  	/* projection parameters validity flag         */
#define DDPUV 4	  	/* ground units validity flag		       */
#define DDPDV 5	  	/* ground distance validity flag	       */
#define DDCCV 6	  	/* corner coordinates validity flag	       */
#define DDINCV  7	/* line/sample increments validity flag        */

/*  Below of Misc. DDR constants
------------------------------------------------------------------------*/
#define E_PROT -2         /* return status of write protected           */
#define INVAL 0	 	  /* invalid validity flag status		*/
#define VALID 1		  /* valid validity flag			*/
#define UNKNOW 2	  /* unknown and equal validity flag		*/
#define BOUND 2 	  /* bounded min/max validity flag              */
#define COMB 1		  /* images are being combined into one band    */
#define NOCOMB 0 	  /* images are not being combined into one band*/

/*  Below are constants used only within the DDR support routines
------------------------------------------------------------------------*/
#define USAME 2		  /* unknown and equal validity flag	 	*/
#define UDIFF 3		  /* unknown and not equal validity flag	*/

#define DISIZE 18	  /* number of integers in record 1 of DDR      */
#define DDSIZE 27	  /* number of doubles in record 2 of DDR       */
#define DBSIZE 2	  /* number of doubles in band record(s) of DDR */

#define DDTSLN 152	  /* length of all strings in band record of DDR*/
#define DDSRLN 32	  /* length of the SOURCE character string      */
#define DDINLN 32	  /* length of the INSTRUMENT character string  */
#define DDDRLN 64	  /* length of the DIRECTION character string   */
#define DDCDLN 10	  /* length of the capture DATE character string*/
#define DDCTLN 8	  /* length of the capture TIME character string*/
#define DDBNLN 4	  /* length of the band no. character string    */
#define DDVLLN 2	  /* length of the valid flag character string  */

#define DDSYLN 12	  /* length of the SYSTEM character string      */
#define DDPULN 12	  /* length of the PROJ_UNITS character string  */
#define DDLDLN 12	  /* length of the LAST_USED_DATE char. string  */
#define DDLTLN 12	  /* length of the LAST_USED_TIME char. string  */
#define DDSTCT 4	  /* No. of string in record 1 of DDR file      */
#define DDNVAL 8	  /* No. of valid flags stored in the DDR file  */


#ifndef lasErr
#define lasErr int
#endif

#ifndef FUNCTION
#define FUNCTION
#endif

void FUNCTION set_zone(const struct DDR in_ddr[],struct DDR *out_ddr,int *zone_flag,int nimg);
void FUNCTION set_punit(const struct DDR in_ddr[],struct DDR *out_ddr,int nimg);
void FUNCTION set_proj(const struct DDR in_ddr[],struct DDR *out_ddr,int *proj_flag,int nimg);
void FUNCTION set_ppar(const struct DDR in_ddr[],struct DDR *out_ddr,
		int *ppar_flag,int *proj_flag,int nimg);
void FUNCTION set_pdist(int pdist_flag,const struct DDR in_ddr[],
		struct DDR *out_ddr,double line_inc,double samp_inc);
void FUNCTION set_master(const struct DDR in_ddr[],struct DDR *out_ddr,int inc_flag,int window[]);
void FUNCTION set_inc_flag(const struct DDR ddr[],int *inc_flag,int *inc_index,int nimg);
void FUNCTION set_datum(const struct DDR in_ddr[],struct DDR *out_ddr,int nimg);
void FUNCTION set_corners(int corner_flag,int proj_flag,int pdist_flag,int zone_flag,int ppar_flag,
		     const struct DDR in_ddr[],struct DDR *out_ddr,double line_inc,double samp_inc,int window[]);
void FUNCTION set_capture(const struct BDDR in_bddr[],struct BDDR *out_bddr,int nimg);
void FUNCTION set_pdis_flag(const struct DDR ddr[],int *pdist_flag,int *pdist_index,int nimg);
lasErr FUNCTION set_increment(int inc_flag,const struct DDR *in_ddr,struct DDR *out_ddr,double in_line_inc,double in_samp_inc);
void FUNCTION set_corn_flag(struct DDR ddr[],int *corner_flag,int *window[],
		int *copy_index,int pdist_flag, float tgbl,int pdist_index,int gbl_flag,int nimg);
void FUNCTION com_ppar(struct DDR ddr1,struct DDR ddr2,int *ppar_flag,
	int default_flag,int image1,int image2);

double packed_deg(double deg);/*Convert from packed degree format used by
LAS.*/

lasErr c_getddr(const char *hname,struct DDR *ddr);
void FUNCTION c_intddr(struct DDR *ddr);
lasErr c_putddr(const char *hname,struct DDR *ddr);

/*BDRs are now written by the c_putddr routine.  These routines are depricated*/
lasErr int_c_getbdr(const char  *hname,struct BDDR *bddr, int  *band);
void FUNCTION int_c_intbdr(struct BDDR *bddr);
lasErr FUNCTION int_c_putbdr(const char *hname,const struct BDDR *bddr);
/*End depricated.*/

void FUNCTION comp_sm_esq(struct DDR ddr1,struct DDR ddr2,int *ppar_flag,
	int default_flag,int image1,int image2);
void FUNCTION comp_prec(double ddr1_value,double ddr2_value,int   *ppar_flag,
	int   prec,int   default_flag,int   image1,int   image2);
void FUNCTION com_ppar(struct DDR ddr1,struct DDR ddr2,int *ppar_flag,
	int default_flag,int image1,int image2);

/*Get per-pixel data size from data type.*/
#define DTYPE_BYTE 1
#define DTYPE_SHORT 2
#define DTYPE_LONG 3
#define DTYPE_FLOAT 4
#define DTYPE_DOUBLE 5
#define DTYPE_COMPLEX 10
#define DTYPE_COMPLEXREAL 11
#define DTYPE_COMPLEXIMAG 12

int dtype2dsize(int dtype,char **description);
/*Read line from file.*/
void getFloatLine(FILE *f,const struct DDR *ddr,int yLine,float *dest); 
void getFloatLine_mb(FILE *f,const struct DDR *ddr,
	int yLine,int band,float*dest); 
/*Write line to file.*/
void putFloatLine(FILE *f,const struct DDR *ddr,int yLine,const float *source);
void putFloatLine_mb(FILE *f,const struct DDR *ddr,
	int yLine,int band,const float *source);

#endif

