/*  This is a general WORLD include file.  The constants within this include 
    file are used by many if not all the LAS support and application routines
------------------------------------------------------------------------------*/
/* Boolean true and false 
-------------------------*/
#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif

/* define NULL 
--------------*/
#ifndef NULL
#define NULL 0
#endif

/* SUCCESS or FAIL return status,
   End of File, & Buffer to small
---------------------------------*/
enum {
	E_SUCC=0,
	E_FAIL=-1,
	E_EOF=-2,
	E_SMAL=-3
};
#ifndef lasErr
#define lasErr int
#endif

/* character string lengths
---------------------------*/
#define ERRLEN 256
#ifndef CMLEN
#define CMLEN 256
#endif

/* valid data types 
-------------------*/
#define EBYTE 1
#define EWORD 2
#define ELONG 3
#define EREAL 4
#define EDOUBLE 5
#define ECHAR 6
#define EUWORD 7
#define EULONG 8
#define NDTYPES 8
	
/* file access types 
--------------------*/
#define IREAD 0
#define IWRITE 1
#define IUPDATE 2

/*  define the key word FUNCTION  -- This is placed at the beginning of each 
    function declared in 'C' in order to designate the functions from each other
------------------------------------------------------------------------------*/
#ifndef FUNCTION		/*  if not defined			      */
#define FUNCTION		/*  define FUNCTION			      */
#endif


void print_mes(char *,char *);
#define LAS_FATAL -1
#define NON_FATAL 1
void c_errmsg(char	* message,char	* key, int	severity);
char * c_getsys(void);
FUNCTION char *squeeze(register const char *str,register int  len);
void FUNCTION c_up2low(register char *buf, register int   *size);
void c_low2up(register char *buf, register int *size);
lasErr FUNCTION c_sysset(const char *sysName,int *sysNumber);
void FUNCTION c_sigblk(int *mask);
void FUNCTION c_sigunblk(int *mask);
lasErr FUNCTION c_pxconv(int ftyp,int totyp, const unsigned char *frombuf,unsigned char * tobuf, int size);
void FUNCTION c_pxcopy(const unsigned char *frombuf, unsigned char *tobuf, int dtype,int ns);
lasErr FUNCTION c_pxsys(int insys, unsigned char *buf,int dtype,int size);
void FUNCTION c_pxfill(unsigned char *tobuf,int * dtype, int *ns, unsigned char *fillval);
lasErr FUNCTION c_pxswap(unsigned char *buf, int ns, int size);

lasErr FUNCTION c_rtcoef(const double proj[8],const  int image[8],double coef[6], int *flag);
lasErr FUNCTION c_rotcor(const double proj[8],const  int image[8], double window[4],double proj_wind[8]);
void FUNCTION c_sigblk(int *mask);
void FUNCTION c_sigunblk(int *mask);
	
#ifndef FILE
	#include "asf.h"
#endif
lasErr FUNCTION c_lsopen ( FILE **fd, const char *hname, int *rwmode);
lasErr FUNCTION c_lsread(FILE **fd,const char * key, int *clen, int *dlen, 
		char *cbuf, unsigned char *dbuf, char *dtype);
lasErr FUNCTION c_lsrepl (FILE **fd, const char *key, int *clen, int *dlen,
		 const char *cbuf, const unsigned char *dbuf, const char *dtype);
lasErr FUNCTION c_lswrit(FILE	**fd, const char *key, int	*clen, int	*dlen, 
		const char *cbuf, const unsigned char *dbuf, const char *dtype);
lasErr FUNCTION c_lsstat (const char *hostname, int *mode);
void FUNCTION c_lsmknm(const char *inname,const char *ext,char *outname);
lasErr FUNCTION c_lsclos (FILE **fd, const char *hostname, int *action);
FUNCTION char *squeeze(register const char *str,register int  len);
double ecc_sq(double semi_major,double semi_minor);
FUNCTION double smajor_tbl(int datum);
double FUNCTION sminor_tbl(int datum);








