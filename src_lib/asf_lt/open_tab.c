/*******************************************************************************
FUNCTION:	open_tab

PURPOSE:	Opens the Labeled Table file with the specified access.
		It initializes the TAB_DEF structure by allocating the
		I/O buffers and assigning appropriate values to each of
		the structure fields.  If opened for append and the file
		already exists, error checking is done to ensure the
		subfile list and LT file types are the same.
PROGRAM HISTORY:
  Version  Date   Author        Request
  -------  ----   ------        -------
  5.0     ?????   ?????
  5.1     03/93   D. Etrheim    Changed pmode to 666 for file open.
                                This will allow users to use umask
                                appropriately. Execute permision is not
                                needed for any files created by LAS.


*******************************************************************************/
#include "asf.h"


#include <unistd.h>
#include <fcntl.h>

#include "worgen.h"
#include "ltable.h"

lasErr sktab(int *fd,int *nbytes,int *pos);
lasErr optab(int *fd,char *hname,int *mode);

void open_tab(
    struct TAB_DEF *tab,	/* struct of Labeled Table 		      */
    char *fname,		/* Host name of the LT file to be opened      */
    int access)		/* type of access to open the file with       */
				/*   0 - read				      */
				/*   1 - write				      */
				/*   2 - append				      */
{
#define DIR_END '/'

int dirlen;
int eor;
int flag;
int index;
int index1;
int nsubs;
int two = 2;
int zero = 0;
char *ftype;
char msg[ERRLEN];
char name[CMLEN];
char sep;
char *subfile[MAX_SUB];
char *val;

if (optab(&tab->fd,fname,&access) != E_SUCC) 	/* open the LT file       */
   {
   sprintf(msg,"Error opening Labeled Table file \"%s\".",fname);
   tab_error(tab,TAB_LT_OPEN,FATAL,msg,"open_tab");
   return;
   }

strcpy(tab->hname,fname);		/* initialize the TAB_DEF structure   */
tab->access = access;
tab->buf.addr = MALLOC(TAB_SIZ);

*(tab->buf.addr) = '\0';
tab->buf.length = TAB_SIZ;
tab->vector = NULL;
tab->nbytes = 0;
tab->cur = 0;
tab->currec = 2;
flag = access;
if (access == 2)			/* if opened for append access, issue */
   {					/*   a physical read.  If the file is */
   read_tab(tab);			/*   empty, treat it as if it is being*/
   if (tab->nbytes == 0)		/*   opened for write access.         */
      flag = 1;
   }

if (flag == 0)				/* if opened for read access          */
   {					/*   read the list of subfiles, attach*/
   read_tab(tab);			/*   the directory name, and store in */
   if (tab->nbytes == 0)		/*   LT structure.                    */
      {
      tab_error(tab,TAB_LT_OPEN,FATAL,
                "Labeled Table is opened for read access and contains no data.",
                "open_tab");
      return;
      }

   strcpy(name,fname);
   if ((val = strrchr(name,DIR_END)) == NULL)
      *name = '\0';
   else
      *(val + 1) = '\0';

   dirlen = strlen(name);
   for (index=0; index<MAX_SUB; index++)
      tab->subfile[index] = NULL;

   for (eor=FALSE,tab->nsubs=0; eor!=TRUE; tab->nsubs++)
      {
      val = get_string_field(tab,&eor);
      if (*val == '\0')
         break;

     tab->subfile[tab->nsubs] = MALLOC(dirlen + strlen(val) + 1);
      sprintf(tab->subfile[tab->nsubs],"%s%s",name,val);
      free(val);
      }

   tab->ftype = get_string_field(tab,&eor);	/* store LT file type         */
   tab->fdesc = get_string_field(tab,&eor);	/* store LT file description  */
   tab->label_flag = TRUE;			/* label vector exists        */
   }
else if (flag == 1)			/* if opened for write access         */
   {					/*   remove directory specification   */
   if (tab->nsubs == 0)			/*   from the subfile names and write */
      put_null_field(tab,RECORD_SEP);	/*   them to the master LT.           */
   else
      {
      for (sep=FIELD_SEP1,index=0; index<tab->nsubs; index++)
         {
         if (index+1 == tab->nsubs)
            sep = RECORD_SEP;

	 strcpy(name,tab->subfile[index]);
         if ((val = strrchr(name,DIR_END)) == NULL)
 	    val = name;
 	 else
	    ++val;

         put_string_field(tab,val,sep,TRUE);
         }

      for (index=tab->nsubs; index<MAX_SUB; index++)
         tab->subfile[index] = NULL;
      }

   put_string_field(tab,tab->ftype,FIELD_SEP1,TRUE);	/* write LT file type */
   put_string_field(tab,tab->fdesc,RECORD_SEP,TRUE);	/* write LT file desc */
   tab->label_flag = FALSE;			/* label vector does not exist*/
   }

else if (flag == 2)			/* if opened for append access        */
   {
   for (index=tab->nsubs; index<MAX_SUB; index++)
      tab->subfile[index] = NULL;

   for (eor=FALSE,nsubs=0; eor!=TRUE; nsubs++)
      {
      subfile[nsubs] = get_string_field(tab,&eor);
      if (*subfile[nsubs] == '\0')
         break;
      }

   for (index=0; index<tab->nsubs; index++)	/* error check the list of    */
      {						/*   subfiles to ensure no    */
      for (index1=0; index1<nsubs; index1++)	/*   new ones are specified.  */
         {
         if (strcmp(tab->subfile[index],subfile[index1]) == 0)
            break;
         }

      if (index1 == nsubs)
         {
         sprintf(msg,"%s is not a subfile of the Labeled Table \"%s\".",
                 tab->subfile[index],tab->hname);
         tab_error(tab,TAB_LT_SUBFIL,FATAL,msg,"open_tab");
         return;
         }
      }

   ftype = get_string_field(tab,&eor);		/* error check the file type  */
   if (strcmp(tab->ftype,ftype) != 0)
      {
      sprintf(msg,"%s\"%s\"%s\"%s\"%s\"%s\".",
              "Labeled Table ",tab->hname," has a file type of ",ftype,
	      ", but the program expects a file type of ",tab->ftype);
      tab_error(tab,TAB_LT_FTYPE,FATAL,msg,"open_tab");
      return;
      }

   tab->fdesc = get_string_field(tab,&eor);
   get_vector(tab);				/* retrieve the label vector  */
   tab->label_flag = TRUE;			/* label vector exists        */
   if (sktab(&tab->fd,&zero,&two) != E_SUCC)	/* skip to end of file        */
      {
      tab_error(tab,TAB_LT_SEEK,FATAL,"","open_tab");
      return;
      }

   *(tab->buf.addr) = '\0';
   tab->nbytes = 0;
   tab->cur = 0;
   }

return;
}

/******************************************************************************
FUNCTION:	optab

ALGORITHM:	Opens the Labeled Table file with the specified access.
******************************************************************************/
lasErr optab(
    int *fd, 			/* File descriptor array	   (output)   */
    char *hname,		/* Host file name of Labeled Table (input)    */
    int *mode)			/* File acess.			   (input)    */				/*   0 - read access			      */
				/*   1 - write access			      */
				/*   2 - update access			      */
{
int	flags;
int	pmode = 0666;
char 	msg[ERRLEN];

switch (*mode)	/* set up the access flags for the open call          */
   {
   case 0: flags = O_RDONLY;					break;
   case 1: flags = O_WRONLY | O_TRUNC | O_APPEND | O_CREAT;	break;
   case 2: flags = O_RDWR | O_CREAT;				break;
   default: tab_error(NULL,TAB_LT_ACCESS,NONFATAL,"","open_tab");
            return(E_FAIL);
            break;
   }

if ((*mode == 1) && (access(hname,0) != -1))
   {
   sprintf(msg,"Labeled Table \"%s\" already exists",hname);
   c_errmsg(msg,"open_tab-exists",NON_FATAL);
   return(E_FAIL);
   }

if ((*fd = open(hname,flags,pmode)) == -1)	/* Open the LT file          */
   return(E_FAIL);

return(E_SUCC);
}

/******************************************************************************
FUNCTION:	sktab

PURPOSE:	Resets the file pointer to the specified position on the
		specified Labeled Table file.
******************************************************************************/
lasErr sktab(
    int *fd,		/* File descriptor array 	(input) 	      */
    int *nbytes,	/* Number of bytes to skip	(input)		      */
    int *pos)		/* Position of the file pointer prior to the skip     */
			/*   0 - beginning of the file			      */
			/*   1 - current position of the file pointer	      */
			/*   2 - end of the file			      */
{
if (lseek(*fd,*nbytes,*pos) == -1)
   return(E_FAIL);

return(E_SUCC);
}
