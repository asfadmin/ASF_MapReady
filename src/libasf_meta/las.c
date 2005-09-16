/* Created by 'cat c_*.c' 
   Should have all LAS functionality that we need for reading and writing DDR files */

#include "libasf_meta.h"
#include <ctype.h>
#include "las.h"
#include <math.h>
#include <unistd.h>

#define MSG_PART_LEN 80
#define LSH17 (IEEE_pow[17 + IEEE_R4_EXCESS])   /*           0x20000 */
#define LSH21 (IEEE_pow[21 + IEEE_R4_EXCESS])   /*          0x200000 */
#define LSH32 (IEEE_pow[32 + IEEE_R4_EXCESS])   /*       0x100000000 */
#define LSH48 (IEEE_pow[48 + IEEE_R4_EXCESS])   /*   0x1000000000000 */
#define LSH53 (IEEE_pow[53 + IEEE_R4_EXCESS])   /*  0x20000000000000 */
#define LSH56 (IEEE_pow[56 + IEEE_R4_EXCESS])   /* 0x100000000000000 */


void c_errmsg(char	* message,char	* key, int	severity )
     
{
  int	vflag;			/* int for changing the value of vrity to be */
				/* pass to exit			      */
  char	os_mes[81];	/* message for OS dependent error */
  
  /* Resolve the type of key. */
  if ( key[0] == '\0' )			/* null string check */
    if ( severity <= 0 )			   /* fatal error */
      print_mes("Fatal error encountered.","ERROR-FATAL");
    else
      print_mes("Non-fatal error encountered.","ERROR-NONFATAL");
  
  else if ( key[0] == ' ')
    if ( severity <= 0 )			   /* fatal error */
      print_mes(message,"general-error");
    else
      print_mes(message,"informational");
  
  else if ( (strcmp(key,"unix")==0) || (strcmp(key,"UNIX")==0) )
    {
	sprintf(os_mes,"%s%s%d."," UNIX "," error code ",severity); 
  	print_mes(os_mes,"ERROR-UNIX");
    }
  
  else if ( (strcmp(key,"vms")==0) || (strcmp(key,"VMS")==0) )
    {
      sprintf(os_mes,"%s%s%d."," VMS "," error code ",severity); 
      print_mes(os_mes,"ERROR-VMS");
    }
  
  else
    print_mes( message, key );
  
  /* Decide whether or not to continue processing. 
     ------------------------------------------------ */
  if ( severity <= 0 )
    {
      vflag = -1;
      exit(vflag);
    }
  
  return;
}

void print_mes( char	*ms,char	* ky )
{
  short	back;	/* counters to break up string MS */
  char	*newms;		/* pointer to remaining part of ms */
			/* if it is too int for a one line */
  char	restms[CMLEN];	/* string to pass recursively to print_mes */
  
  /* If the length of MS is not greater than MSG_PART_LEN - the length of 
   the key then print it 
   -----------------------------------------------------------------------*/
  if ( strlen(ms) <= (MSG_PART_LEN-3)-strlen(ky) )
    {
      printf("\r[%s] %s\n",ky ,ms);
    }
  else
    {
      for ( back = (MSG_PART_LEN-3)-strlen(ky);
	    ((back>=0) && (!isspace(ms[back])));
	    back--)
	;		/* find w. space to def substring */
      if ( back <= 0 )
	{
	  for ( back = (MSG_PART_LEN-3)-strlen(ky);
		((back>=0) && (ms[back] != '/') &&
		 (ms[back] != '.') &&
		 (ms[back] != ']'));
		back--)
	    ;		/* no w. space, find host path separator */
	  if ( back <= 0 ) back = (MSG_PART_LEN-3)-strlen(ky);
	}			/*no path separator, just wrap after 80th char*/
      newms = (ms + back + 1);
      strcpy(restms,newms);
      ms[back + 1] = '\0';	/* define substr with null */
      printf("\r[%s] %s\n",ky ,ms);
      print_mes( restms, ky );		/* print rest of string */
    }
  
  return;
}

lasErr int_c_getbdr(const char  *hname,struct BDDR *bddr, int  *band)
{
  struct DDR ddr;		       /* working DDR structure			      */
  
  int	access;		       /* file access type			      */
  int	action;		       /* file close action flag		      */
  int	clen;		       /* length of character part of record	      */
  int	ctemp;		       /* temporary storage for clen		      */
  int	dlen;		       /* length of data part of record	      	      */
  int	dtemp;		       /* temporary storage for dlen		      */
  FILE *	fd;		       /* file descriptor for opened file	      */
  /*int    status;*/
  int	size = DBSIZE;	       /* number of doubles in the BDR		      */
  int	dtyp = EDOUBLE;        /* data type of the buffer to be converted     */
  int	convflg = FALSE;       /* working conversion flag		      */
  int	syscode;	       /* DDR system code			      */
  
  unsigned char *dbuf;	       /* pointer to area where data is stuffed       */
  
  char dtype[3];	       	       /* type of data portion of record	      */
  char hostddr[CMLEN];           /* host name of DDR file			      */
  char key[16];	       	       /* record key				      */
  char tempchar[DDTSLN];	       /* character portion of the record	      */
  char *tempptr;
  
  c_lsmknm(hname,".ddr",hostddr);	        /* make associated DDR file name */
  
  access = 0;				/* open DDR file for read */
  c_lsopen(&fd,hostddr,&access);
  
  clen = DDSTCT * DDSYLN;	       /* set up and read record 1 of the DDR */
  ctemp = clen - 1;
  dlen = dtemp = DISIZE * 4;
  dbuf = (unsigned char *) &ddr;
  c_lsread(&fd,"DDRINT",&clen,&dlen,ddr.system,dbuf,dtype);
  
  if ((clen != ctemp) || (dlen != dtemp))
    {
      c_errmsg("Error reading record of associated DDR file","getbdr-read",NON_FATAL);
      return(E_FAIL);
    }
  
  if (strcmp(c_getsys(),ddr.system) != 0)	/* if the DDR system and the host    */
    convflg = TRUE;			/* system are not the same then set  */
					/* the conversion flag		     */
  
  sprintf(key,"BAND%d",*band);   /* set up for and read in specified band record*/
  clen = DDTSLN;
  ctemp = clen - 1;
  dlen = dtemp = DBSIZE * 8;
  dbuf = (unsigned char *) &(bddr->minval);
  c_lsread(&fd,key,&clen,&dlen,tempchar,dbuf,dtype);
  
  if ( (clen != ctemp) || (dlen != dtemp) )
    {
      c_errmsg("Error reading record of associated DDR file","getbdr-read",NON_FATAL);
      return(E_FAIL);
    }
  
  if (convflg)				/* if the conversion flag is set */
    {
      if (c_sysset(ddr.system,&syscode) != E_SUCC) /* retrieve the DDR   */
	{ c_errmsg("Error returned from sysset","getbdr-call",NON_FATAL);return(E_FAIL);}
      
      if (c_pxsys(syscode,dbuf,dtyp,size) != E_SUCC) /* perform the conversion   */
	{ c_errmsg("Error returned from pxsys","getbdr-call",NON_FATAL);return(E_FAIL); }
    }
  
  sscanf(tempchar,"%4d%2d",&(bddr->bandno),&(bddr->valid));
  tempptr = tempchar + DDBNLN + DDVLLN;
  strcpy(bddr->source,tempptr);
  tempptr += DDSRLN;
  strcpy(bddr->instrument,tempptr);
  tempptr += DDINLN;
  strcpy(bddr->direction,tempptr);
  tempptr += DDDRLN;
  strcpy(bddr->date,tempptr);
  tempptr += DDCDLN;
  strcpy(bddr->time,tempptr);
  
  action = 0;				 /* close associated DDR file */
  c_lsclos(&fd,hostddr,&action);
  
  return(E_SUCC);
}

/* PROTOTYPE from meta_init.c */
void add_meta_ddr_struct(const char *name, meta_parameters *meta, struct DDR *ddr);


lasErr c_getddr(const char *hname,struct DDR *ddr)
{
  int   access;           /* file access type                        */
  int   action;           /* file close action flag                  */
  int   clen;             /* length of char part of record           */
  int   ctemp;            /* temporary storage for clen              */
  int   dlen;             /* length of data part of record           */
  int   dtemp;            /* temporary storage for dlen              */
  FILE *fd;               /* file descriptor for opened file         */
  int   len;
  int   mode;             /* mode for lsstat check                   */
  int   status;           /* function return status code             */
  int   size = DDSIZE;    /* number of doubles in record 2 of DDR    */
  int   dtyp = EDOUBLE;   /* data type of the buffer to be converted */
  int   convflg = FALSE;  /* working conversion flag                 */
  int   syscode;          /* DDR system code                         */
  char  dtype[3];         /* type of data portion of record          */
  char  hostddr[CMLEN];   /* host name of DDR file                   */
  char  temp[2];          /* temporary buffer                        */
  unsigned char *dbuf;    /* pointer to area where data is stuffed   */
  
  c_lsmknm(hname,".ddr",hostddr);      /* make associated DDR file name */
  
  mode = 0;                        /* make sure DDR file exists on disk */
  status = c_lsstat(hostddr,&mode);
  if (status != E_SUCC) {
    fprintf(stderr,"****************** ERROR! ***************\n"
	    "Couldn't open DDR file named '%s'.\n"
	    "Does the file exist?  Is it readable?\n",hostddr);
    exit(99);
  }
  
  access = 0;                                 /* open DDR file for read */
  if (c_lsopen(&fd,hostddr,&access) != E_SUCC) {
    c_errmsg("Error returned from lsopen","getddr-call",NON_FATAL);
    return(E_FAIL);
  }
  
  clen = DDSTCT * DDSYLN;            /* set up for and read in record 1 */
  ctemp = clen - 1;
  dlen = dtemp = DISIZE * 4;
  dbuf = (unsigned char *) MALLOC(dlen);
  if (c_lsread(&fd,"DDRINT",&clen,&dlen,ddr->system,dbuf,dtype) != E_SUCC) {
    c_errmsg("Error returned from lsread","getddr-call",NON_FATAL);
    return(E_FAIL);
  }
  if ( (clen != ctemp) || (dlen != dtemp) ) {
    c_errmsg("Error reading record 1 of associated DDR file",
	     "getddr-read", NON_FATAL);
    return(E_FAIL);
  }
  byte2intArr(dbuf,(int *)ddr,DISIZE);
  free(dbuf);
  
  clen = ctemp = 0;                  /* set up for and read in record 2 */
  dlen = dtemp = DDSIZE * 8;        /* no character part for this record*/
  dbuf = (unsigned char *) &(ddr->proj_coef[0]);
  if (c_lsread(&fd,"DDRDUB",&clen,&dlen,temp,dbuf,dtype) != E_SUCC) {
    c_errmsg("Error returned from lsread","getddr-call",NON_FATAL);
    return(E_FAIL);
  }
  if ( (clen != ctemp) || (dlen != dtemp) ) {
    c_errmsg("Error reading record 2 of associated DDR file",
	     "getddr-read",NON_FATAL);
    return(E_FAIL);
  }
  
  if (strcmp(c_getsys(),ddr->system) != 0) /* if the DDR system and the */
    convflg = TRUE;                /* host are not the same then  */
  /* set the conversion flag     */
  
  if (convflg) {                       /* if the conversion flag is set */
    if (c_sysset(ddr->system,&syscode) != E_SUCC) { /* retrieve the DDR*/
      c_errmsg("Error returned from sysset","getddr-call",NON_FATAL);
      return(E_FAIL);
    }
    
    /* perform the conversion */
    if (c_pxsys(syscode,dbuf,dtyp,size) != E_SUCC) {
      c_errmsg("Error returned from pxsys","getddr-call",NON_FATAL);
      return(E_FAIL);
    }
  }
  
  /* Change the projection units to lower case */
  len = strlen(ddr->proj_units);
  c_up2low(ddr->proj_units,&len);
  
  action = 0;                              /* close associated DDR file */
  if (c_lsclos(&fd,hostddr,&action) != E_SUCC) {
    c_errmsg("Error returned from lsclos","getddr-call",NON_FATAL);
    return(E_FAIL);
  }
  
  /* Remember the name and location of the ddr struct 
  add_meta_ddr_struct(hname, NULL, ddr);
  return (E_SUCC);*/
}

FUNCTION char *c_getsys(void)
     
{
#if defined(lil_ieee)
  return IEEE_LIL;
#elif defined(cray_float)
  return UNICOS;
#else
  /*#if defined(big_ieee)*/
  return IEEE;
#endif 
}

void FUNCTION int_c_intbdr(struct BDDR *bddr)
{
  bddr->minval = 0.0;
  bddr->maxval = 0.0;
  bddr->bandno = 0;
  bddr->valid = 0;
  strcpy(bddr->source," ");
  strcpy(bddr->instrument," ");
  strcpy(bddr->direction," ");
  strcpy(bddr->date," ");
  strcpy(bddr->time," ");
  
  return;
}  /*  c_intbdr  */

void FUNCTION c_intddr(struct DDR *ddr)
{
  int i;				/* loop index				      */
  
  ddr->nl = 0;
  ddr->ns = 0;
  ddr->nbands = 0;
  ddr->dtype = 0;
  ddr->master_line = 1;
  ddr->master_sample = 1;
  
  /*  Set the validity flags to zero  
      ----------------------------------*/
  for (i = 0; i < DDNVAL; i++)
    ddr->valid[i] = INVAL;
  
  ddr->proj_code = 0;
  ddr->zone_code = 0;char *appendExt(const char *name,const char *newExt);
  ddr->datum_code = 0;
  ddr->spare = 0;
  
  strcpy(ddr->system,c_getsys());
  strcpy(ddr->proj_units," ");
  strcpy(ddr->last_used_date," ");
  strcpy(ddr->last_used_time," ");
  
  /*  Set the 15 projection coeficients to zero
      ---------------------------------------------*/
  for (i = 0; i < 15; i++)
    ddr->proj_coef[i] = 0.0;
  
  ddr->upleft[0] = 0.0;
  ddr->upleft[1] = 0.0;
  ddr->loleft[0] = 0.0;
  ddr->loleft[1] = 0.0;
  ddr->upright[0] = 0.0;
  ddr->upright[1] = 0.0;
  ddr->loright[0] = 0.0;
  ddr->loright[1] = 0.0;
  ddr->pdist_y = 0.0;
  ddr->pdist_x = 0.0;
  ddr->line_inc = 1.0;
  ddr->sample_inc = 1.0;
  
  return;
}

void c_low2up(register char *buf, register int *size)
{
  register int cnt;
  register char *ptr;
  
  cnt = *size;
  ptr = buf;
  for(; cnt--; ptr++)
    {
      if (islower(*ptr))
	*ptr = toupper(*ptr);
    }
  return;
}

lasErr FUNCTION c_lsclos (FILE **fd, const char *hostname, int *action)
{
  FILE *fdtemp;
  int	clen, dlen, total_len, empty,  more, nbytes;
  int	buf_size = 1024;
  char	tempfile[CMLEN];
  char	*buffer;
  char    dtype[TYPL];
  char	header[HDRL];
  char	*key=NULL;
  char	msgtxt[ERRLEN + 1];
  char	*ptr;
  
  switch (*action)
    {
    case 0:					/***  normal close    ***/
      FCLOSE(*fd);
      break;
      
    case 1:					/***  crunch file     ***/
      FSEEK64(*fd, 0, 0);
      
      strcpy(tempfile,hostname);
      strcat(tempfile,"_temp");
      
      fdtemp = FOPEN (tempfile, "w");
      
      buffer = MALLOC((unsigned)buf_size);
      
      empty = 1;
      more = 1;
      while (more)
	{
	  nbytes = fread(header,sizeof(char),HDRL,*fd);
	  if (nbytes == 0)
	    break;
	  
	  if (nbytes == -1)
	    {
	      c_errmsg("Error reading header record from label services file",
		       "lsclos-read",NON_FATAL);
	      return(E_FAIL);
	    }
	  
	  clen = dlen = 0;
	  if (((ptr = strchr(header,'/')) != NULL ) && ((int)(ptr - header) < LENL))
	    sscanf(header,"%d/%d%s",&clen,&dlen,dtype); 
	  else
	    sscanf(header,"%d%s",&dlen,dtype); 
	  
	  key = squeeze(header+LENL+TYPL,strlen(header+LENL+TYPL));
	  total_len = clen + dlen;
	  
	  if (strcmp(key,"DELETED") != 0)		/* copy record	*/
	    {					/* to new file	*/
	      if (total_len > buf_size)
		{
		  free(buffer);
		  buf_size = total_len;
		  
		  buffer = MALLOC((unsigned)buf_size);
		}
	      
	      FREAD(buffer,1,total_len,*fd);
	      
	      FWRITE(header,1,HDRL,fdtemp);
	      FWRITE(buffer,1,nbytes,fdtemp);
	      
	      empty = 0;
	    }
	  else					/* skip record	*/ 
	    {
	      if (fseek(*fd, (int)total_len, 1) == -1)
		{
		  c_errmsg("Error seeking in label services file",
			   "lsclos-seek",NON_FATAL);
		  return(E_FAIL);
		}
	    }
	}
      
      free(buffer);
      
      if (fclose(*fd) != 0)
	{
	  c_errmsg("Error closing label services file","lsclos-close",
		   NON_FATAL);
	  return(E_FAIL);
	}
      
      if (fclose(fdtemp) != 0)
	{
	  sprintf(msgtxt,"Error closing temporary file %s",tempfile);
	  c_errmsg(msgtxt,"lsclos-close",NON_FATAL);
	  return(E_FAIL);
	}
      
      if (unlink(hostname) == -1)
	{
	  sprintf(msgtxt,"Error deleting label services file %s",
		  hostname);
	  c_errmsg(msgtxt,"lsclos-delete",NON_FATAL);
	  return(E_FAIL);
	}
      
      if (!empty)					/***  delete file   ***/
	{
	  if (rename(tempfile,hostname) != E_SUCC)
	    {
	      sprintf(msgtxt,"Error renaming file %s to %s",
		      tempfile,hostname);
	      c_errmsg(msgtxt,"lsclos-rename",NON_FATAL);
	      return(E_FAIL);
	    }
	}
      else
	if (unlink(tempfile) == -1)
	  {
	    sprintf(msgtxt,"Error deleting label services file %s",
		    hostname);
	    c_errmsg(msgtxt,"lsclos-delete",NON_FATAL);
	    return(E_FAIL);
	  }
      
      free(key);
      break;
      
    case 2:					/***  delete file     ***/
      if (unlink(hostname) == -1)
	{
	  sprintf(msgtxt,"Error deleting label services file %s",
		  hostname);
	  c_errmsg(msgtxt,"lsclos-delete",NON_FATAL);
	  return(E_FAIL);
	}
      break;
    }
  
  return(E_SUCC);
}

void FUNCTION c_lsmknm(const char *inname,const char *ext,char *outname)
{
  char *newName=appendExt(inname,ext);
  strcpy(outname,newName);
  free(newName);
  return;
}

lasErr FUNCTION c_lsopen ( FILE **fd, const char *hname, int *rwmode)
{
  FILE 	*fp;
  char    msgtxt[ERRLEN + 1];
  char 	*flags;
  
  if (*rwmode == READ)				/* set flags:		      */
    flags = "r";				/* read only */
  else if (*rwmode == WRITE)			/* write only - append	      */
    flags = "w";
  else if (*rwmode == APPEND)			/* write only - append	      */
    flags = "a";
  else if (*rwmode == RW)				/* read/write		      */
    flags = "r+";
  else
    {
      sprintf(msgtxt,"Invalid open mode specified: %d",*rwmode);
      c_errmsg(msgtxt,"lsopen-mode",NON_FATAL);
      return(E_FAIL);
    }
  fp = FOPEN(hname,flags);
  *fd = fp;
  return(E_SUCC);
}

lasErr FUNCTION c_lsread(FILE **fd,const char * key, int *clen, int *dlen, char *cbuf, 
			 unsigned char *dbuf, char *dtype)
{
  FILE   *fp;
  int   charlen, datalen, obyte;
  int   offset,bytesRead;
  short   keymatch = FALSE;
  char   header[HDRL];
  char   msgtxt[ERRLEN + 1];
  char   *in_key;
  char   *out_key;
  char   *ptr;
  
  fp = (FILE *) *fd;
  /*fseek(fp,0,0);*/
  bytesRead=0;
  in_key = squeeze(key,16);
  do
    {
      charlen = datalen = 0;
      obyte=fread(header,sizeof(char),HDRL,fp);
      if (obyte!=HDRL)
    	return E_EOF;
      
      bytesRead+=HDRL;
      
      if (((ptr = strchr(header,'/')) != NULL) && (ptr - header < LENL))
        sscanf(header,"%d/%d%s",&charlen,&datalen,dtype);
      else
        sscanf(header,"%d%s",&datalen,dtype);
      out_key = squeeze(header+LENL+TYPL,KEYL - 1);
      if ((strlen(in_key) == 0) || (strcmp(in_key," ") == 0) 
	  || (strcmp(in_key,out_key) == 0))
        keymatch = TRUE;
      if (strcmp(out_key,"DELETED") == 0)
	keymatch = FALSE;
      if (keymatch) 
	{
	  /*strcpy(key,out_key);*/
	  if (((*clen <= charlen) && (charlen > 0)) || (*dlen < datalen)) 
	    /* buffers are too small */
	    {
	      if (*clen <= charlen)	/* all string to be null terminated   */
		*clen = charlen + 1;
	      if (*dlen < datalen)
		*dlen = datalen;
	      offset = -(HDRL);
	      FSEEK64(fp,offset,1);
	      return(E_SMAL);
    	    }
	  if (charlen > 0)                         /* read character portion    */
	    {
	      obyte = FREAD(cbuf,sizeof(char),charlen,fp);
	      *clen = obyte;
	      *(cbuf + *clen) = '\0';  
	    }
	  else
	    *clen = 0;
	  if (datalen > 0)                         /* read data portion         */
	    {
	      obyte = FREAD(dbuf,sizeof(char),datalen,fp);
	      *dlen = obyte;
	    }
	  else
	    *dlen = 0;
	}
      else
	{
	  offset = datalen + charlen;
	  FSEEK(fp,offset,1);
	  bytesRead+=offset;
	}
      free(out_key);
    }
  while (!keymatch);
  if (*clen + *dlen != charlen + datalen)
    {
      c_errmsg("Error reading from label services file","lsread-read",NON_FATAL);
      sprintf(msgtxt,"    %d bytes requested, %d bytes transferred",
	      charlen + datalen,*clen + *dlen);
      c_errmsg(msgtxt,"lsread-bytes",NON_FATAL);
      return(E_FAIL);
    }
  free(in_key);
  
  return(E_SUCC);
}

lasErr FUNCTION c_lsstat (const char *hostname, int *mode)
{
#ifndef macOS
  if (access(hostname,*mode) != 0)		/* success   */
    return(E_FAIL);
#else
  FILE *f=fopen(hostname,"r");
  if (f==NULL)
    return(E_FAIL);
#endif
  return(E_SUCC);
}

lasErr FUNCTION c_lswrit(FILE	**fd, const char *key, int	*clen, int	*dlen, 
			 const char *cbuf, const unsigned char *dbuf, const char *dtype)
{
  FILE *fp;
  char header[HDRL];
  char len[LENL];
  
  fp = *fd;
  if (strlen(key) >= KEYL)
    {
      c_errmsg("Error: Key length too long.  Key should be truncated.",
	       "lswrit-key", NON_FATAL);
    }
  
  /*Seek to end-of-file.*/
  FSEEK64(*fd, 0, 2);

  if (*clen > 0)
    sprintf(len,"%-d/%-d",*clen,*dlen);
  else
    sprintf(len,"%-d",*dlen);

  sprintf (header,"%-*s%-*s%-*s",LENL,len,TYPL,dtype,KEYL-1,key);
  FWRITE(header,sizeof(char),HDRL,fp);
  
  if (*clen > 0)
    FWRITE(cbuf,sizeof(char), *clen,fp);
  
  if (*dlen > 0)
    FWRITE(dbuf,sizeof(char), *dlen,fp);

  return(E_SUCC);
}

lasErr FUNCTION int_c_putbdr(const char *hname,const struct BDDR *bddr)
{
  int	access;		       /* file access type			      */
  int	action;		       /* file close action			      */
  int	clen;		       /* length of char part of record	      	      */
  int	dlen;		       /* length of data part of record	      	      */
  FILE *	fd;		       /* file descriptor			      */
  
  unsigned char *dbuf;	       /* pointer to area where data is stuffed       */
  
  char hostddr[CMLEN];           /* host name of DDR file			      */
  char key[16];	       	       /* record key				      */
  char tempchar[DDTSLN];	       /* temp. storage of char. portion or record    */
  char *tempptr;
  
  /* Ensure that a valid band number was specified.
     -------------------------------------------------*/
  
  if ((bddr->bandno < 1) || (bddr->bandno > MAXBND))
    {
      c_errmsg("Incorrect band number specified for BDDR","putbdr-badnum",NON_FATAL);
      return(E_FAIL);
    }
  
  c_lsmknm(hname,".ddr",hostddr);
  
  /* Open DDR file for write access 
     ---------------------------------*/
  access = 2;				 
  if (c_lsopen(&fd,hostddr,&access) != E_SUCC)
    {
      c_errmsg("Error returned from lsopen","putbdr-call",NON_FATAL);
      return(E_FAIL);
    }
  
  /* Initailize the variables needed the write the band record
     ------------------------------------------------------------*/
  clen = DDTSLN - 1;
  dlen = DBSIZE * 8;
  dbuf = (unsigned char *) &(bddr->minval);
  sprintf(key,"BAND%d",bddr->bandno);
  
  /*  Place structure members into a character buffer
      ---------------------------------------------------*/
  sprintf(tempchar,"%4d%2d",bddr->bandno,bddr->valid);
  tempptr = tempchar + DDBNLN + DDVLLN;
  strcpy(tempptr,bddr->source);
  tempptr += DDSRLN;
  strcpy(tempptr,bddr->instrument);
  tempptr += DDINLN;
  strcpy(tempptr,bddr->direction);
  tempptr += DDDRLN;
  strcpy(tempptr,bddr->date);
  tempptr += DDCDLN;
  strcpy(tempptr,bddr->time);
  
  c_lswrit(&fd,key,&clen,&dlen,tempchar,dbuf,"R8");
  
  action = 0;				  /* close associated DDR file        */
  c_lsclos(&fd,hostddr,&action);
  
  return(E_SUCC);
}

lasErr c_putddr(const char *hname,struct DDR *ddr)
{
  int   access;                    /* file access type                          */
  int   action;                    /* file close action                         */
  int   clen;                      /* length of char part of record             */
  int   dlen;                      /* length of data part of record             */
  FILE *   fd;                     /* file descriptor                           */
  
  
  unsigned char *dbuf;             /* pointer to area where data is stuffed     */
  
  char d_temp[DDSTCT][DDSYLN];     /* temporary for squeezed strings            */
  char *junk_temp,hostddr[1024];
  
  /* Ensure that required parameters were specified.
     --------------------------------------------------*/
  
  if ((ddr->nl < 1) || (ddr->nl > MAXNL))
    {
      c_errmsg("Invalid number of lines specified","putddr-badnl",NON_FATAL);
      return(E_FAIL);
    }
  
  if ((ddr->ns < 1) || (ddr->ns > MAXNS))
    {
      c_errmsg("Invalid number of samples specified","putddr-badns",NON_FATAL);
      return(E_FAIL);
    }
  
  if ((ddr->nbands < 1) || (ddr->nbands > MAXBND))
    {
      c_errmsg("Invalid number of bands specified","putddr-badbnds",NON_FATAL);
      return(E_FAIL);
    }
  
  if ((ddr->dtype < 1) || (ddr->dtype > 20))
    {
      c_errmsg("Invalid data type specified","putddr-bdtype",NON_FATAL);
      return(E_FAIL);
    }
  
  strcpy(ddr->system,c_getsys());
  
  c_lsmknm(hname,".ddr",hostddr);
  
  access = 1;                               /* open DDR file for write access   */
  c_lsopen(&fd,hostddr,&access);
  
  /*  Place the string portion of the DDR into a temporary buffer
      ---------------------------------------------------------------*/
  junk_temp = (char *)&ddr->spare;
  strncpy(junk_temp,ddr->system,4);
  strcpy(d_temp[0],squeeze(ddr->system,DDSYLN));
  strcpy(d_temp[1],squeeze(ddr->proj_units,DDPULN));
  strcpy(d_temp[2],squeeze(ddr->last_used_date,DDLDLN));
  strcpy(d_temp[3],squeeze(ddr->last_used_time,DDLTLN));
  
  clen = DDSTCT * DDSYLN - 1;               /* set up and output record 1       */
  dlen = DISIZE * 4;
  dbuf = (unsigned char *) MALLOC(dlen);
  int2byteArr((int *)ddr,dbuf,DISIZE);
  c_lswrit(&fd,"DDRINT",&clen,&dlen,d_temp[0],dbuf,"I4");
  free(dbuf);
  
  /* Set up and output record 2
     There is no character part to this record 
     --------------------------------------------*/
  clen = 0;                                 
  dlen = DDSIZE * 8;                
  dbuf = (unsigned char *) &(ddr->proj_coef[0]);
  c_lswrit(&fd,"DDRDUB",&clen,&dlen,d_temp[0],dbuf,"R8");
  
  action = 0;                               /* close associated DDR file        */
  c_lsclos(&fd,hostddr,&action);
  
  {/*Create/write correct number of BDRs*/
    int bandNo;
    for (bandNo=1;bandNo<=ddr->nbands;bandNo++)
      {
	struct BDDR bdr;
	int_c_intbdr(&bdr);
	bdr.bandno=bandNo;
	int_c_putbdr(hname,&bdr);
      }
  }
  return(E_SUCC);
}

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

lasErr FUNCTION c_pxswap(unsigned char *buf, int ns, int size)
{
  register unsigned char *bottom;
  register unsigned char *top;
  register unsigned char temp;
  register int count;
  
  if (size == 1)
    return(E_SUCC);
  else if ((size % 2) != 0)
    {
      c_errmsg("Odd number of bytes per entity specified",
	       "pxswap-oddnum", LAS_FATAL);
      return(E_FAIL);
    }
  else
    {
      count = ns;
      for (; count--; buf += size)
	{
	  bottom = buf;
	  top    = bottom + size - 1;
	  while (bottom < top)
	    {
	      temp = *bottom;
	      *(bottom++) = *top;
	      *(top--) = temp;
	    }
	}
    }
  return(E_SUCC);
}

lasErr FUNCTION c_sysset(const char *system,int *nsys)
     /* character coded system (e.g. "gould-utx") */
     /* system numerical code (e.g. SYS_GOULD_UTX)*/
{
  /*int len;*/			/* Length of input system string	      */
  int index;			/* Looping variable			      */
  lasErr status = E_FAIL;		/* Return status			      */
  
  static char *sys_name[] = 	/* Array of valid system strings	      */
    {IEEE,IEEE_LIL,MVS,UNICOS,MSC,NULL};
  
  /*Find the input system string in the list of valid system strings*/
  for (index = 0; sys_name[index] ; index++)
    if (strcmp(system,sys_name[index]) == 0)
      {
	*nsys = index;
	status=E_SUCC;
	break;
      }
  
  return(status);
}

void FUNCTION c_up2low(register char *buf, register int   *size)
{
  register int cnt;
  register char *ptr;
  
  cnt = *size;
  ptr = buf;
  for(; cnt--; ptr++)
    {
      if (isupper(*ptr))
	*ptr = tolower(*ptr);
    }
  return;
}

void byte2intArr(unsigned char *inBuf,int *outArr,int nInts)
{
  int i;
  for (i=0;i<nInts;i++)
    outArr[i]=((int)inBuf[i*4+0])<<24|
      ((int)inBuf[i*4+1])<<16|
      ((int)inBuf[i*4+2])<<8|
      ((int)inBuf[i*4+3])<<0;
}

void int2byteArr(int *inArr,unsigned char *outBuf,int nInts)
{
  int i;
  for (i=0;i<nInts;i++)
    {
      outBuf[i*4+0]=0xff&(inArr[i]>>24);
      outBuf[i*4+1]=0xff&(inArr[i]>>16);
      outBuf[i*4+2]=0xff&(inArr[i]>>8);
      outBuf[i*4+3]=0xff&(inArr[i]>>0);
    }
}

FUNCTION char *squeeze(register const char *str,register int  len)
{
  register char *newstr;
  register char *ptr;
  
  newstr = MALLOC(len+1);
  
  strncpy(newstr,str,len);
  for(ptr = newstr + len - 1; ((ptr >= newstr) && (*ptr == ' ')); ptr--)
    ;
  *(ptr + 1) = '\0';
  return(newstr);
}

/* function declarations */
double sgn(double);

/* constants */
#define SMALL  1.0e-10    /* small floating number */

double packed_deg(x)
double x; 
{


   double sign;
   int deg;  /* degrees */
   int min;  /* minutes */
   double sec;  /* seconds */
   double q;

   sign=sgn(x);
   deg = (int)(x=fabs(x)+SMALL);
   min = (int)(q=(x-(double)deg)*60.0+SMALL);
   sec = ((double)q-min)*60.0+SMALL;

   if (sec > 59.95) {
     sec = 0.0;
     min++;
     if (min > 59) {
       min = 0;
       deg++;
     }
   }

   return ((double)(sign * ((deg * 1.0e+6) + (min * 1.0e+3) + sec)));
}

double sgn(x)
  double x;
{
  if (x >= 0.0)
    return 1.0;

  return (-1.0);
}
