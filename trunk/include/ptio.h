#ifndef _PTIO_H_
#define _PTIO_H_

typedef lasErr (*ptRWroutine)(FILE *fp,int *pt_data);
struct PTDESC
   {
   FILE * fptr;		/* File pointer */
   int acc;		/* Access mode:  (IREAD, IUPDATE, or IWRITE) */
   ptRWroutine routine;	/* Pointer to read/write routine */
   char name[CMLEN];	/* Name of tie point file */
   };

lasErr FUNCTION c_ptio(struct PTDESC **ptdesc, struct TPSDATA *pt_data);
lasErr FUNCTION c_ptopen(struct PTDESC **ptdesc, const char *fname,int * filetype, 
		int *acc, struct TPSHEAD *header);
lasErr c_ptclse (struct PTDESC **ptdesc); 

#endif
