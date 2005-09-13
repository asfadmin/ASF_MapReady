/******************************************************************************
FUNCTION:	error

PURPOSE: 	Utility routine for Labeled Table programs to process
		error conditions.  

PROGRAM HISTORY:
  Version	Date       Author       Request
  -------	----	   ------       -------
    1.0         12/86      K. Gacke     original development
    1.1          1/87	   K. Gacke 	new LT file format
    2.0         12/87      B. Ailts     Change include directory specifications
    2.1		05/88	   B. Ailts	Standardized error messages
  					Changed error status TRUE to E_SUCC
					Changed error status FALSE to E_FAIL
					Removed key as an argument

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:	none.

PROJECT:	LAS

ALGORITHM:
Verify and reset the severity flag, error code and status values if needed
Route to the appropriate error message

ALGORITHM REFERENCES:
******************************************************************************/

#include "asf.h"


#include "worgen.h"
#include "ltable.h"

int tabmode = 1;	/* Error mode flag                    */
			/*     1  => LT aborts on fatal errors                */
			/*   !(1) => LT returns control to calling program    */

int tabstat = 1;	/* LT error status code                               */
			/*     1  => LT utility completed successfully        */
			/*   !(1) => LT utility encountered fatal error       */

void tab_error(struct TAB_DEF *tab,
    int err_code,int severity,char msg[],char key[])
{
int status;

char err_buf[30];
char buf[ERRLEN];

if ((severity == FATAL) && (tabmode != 1))
   {
   tabstat = err_code;
   severity = NONFATAL;
   status = NONFATAL;
   }
else
   {
   tabstat = err_code;
   status = severity;
   }

if (strlen(msg))
   status = NONFATAL;

switch (err_code)
   {
   case TAB_LT_OPEN:
	sprintf(err_buf,"%s-%s",key,"open");
	c_errmsg("Error opening Labeled Table file",err_buf,status);
	break;

   case TAB_LT_ACCESS:
	sprintf(err_buf,"%s-%s",key,"access");
	c_errmsg("Invalid Labeled Table file access specified",err_buf,
                status);
	break;

   case TAB_LT_READ:
	sprintf(buf,"Error in Labeled Table \"%s\" at logical record %d.",
                tab->hname,tab->currec);
	sprintf(err_buf,"%s-%s",key,"error");
	c_errmsg(buf,err_buf,NON_FATAL);
	sprintf(err_buf,"%s-%s",key,"read");
	c_errmsg("Error reading from the Labeled Table file",err_buf,status);
	break;

   case TAB_LT_WRITE:
	sprintf(buf,"Error in Labeled Table \"%s\" at logical record %d.",
                tab->hname,tab->currec);
	sprintf(err_buf,"%s-%s",key,"error");
	c_errmsg(buf,err_buf,NON_FATAL);
	sprintf(err_buf,"%s-%s",key,"write");
	c_errmsg("Error writing to the Labeled Table file",err_buf,status);
	break;

   case TAB_LT_CLOSE:
	sprintf(buf,"Error closing Labeled Table file \"%s\"",tab->hname);
	sprintf(err_buf,"%s-%s",key,"close");
        c_errmsg(buf,err_buf,status);
	break;

   case TAB_LT_MATRIX_SIZE:
	sprintf(buf,"Error in Labeled Table \"%s\" at logical record %d.",
                tab->hname,tab->currec);
	sprintf(err_buf,"%s-%s",key,"error");
	c_errmsg(buf,err_buf,NON_FATAL);
	sprintf(err_buf,"%s-%s",key,"matrix");
	c_errmsg("Tried to write different matrix sizes into Labeled Table",
                err_buf,status);
	break;

   case TAB_LT_FORMAT:
	sprintf(buf,"Error in Labeled Table \"%s\" at logical record %d.",
                tab->hname,tab->currec);
	sprintf(err_buf,"%s-%s",key,"error");
	c_errmsg(buf,err_buf,NON_FATAL);
	sprintf(err_buf,"%s-%s",key,"format");
	c_errmsg("Invalid Labeled Table file format",err_buf,status);
	break;

   case TAB_LT_DTYPE:
	sprintf(err_buf,"%s-%s",key,"dtype");
	c_errmsg("Invalid Labeled Table data type",err_buf,status);
	break;

   case TAB_LT_EOR:
	sprintf(buf,"Error in Labeled Table \"%s\" at logical record %d.",
                tab->hname,tab->currec);
	sprintf(err_buf,"%s-%s",key,"error");
	c_errmsg(buf,err_buf,NON_FATAL);
	sprintf(err_buf,"%s-%s",key,"EOR");
	c_errmsg("Unexpected end of record encountered",err_buf,status);
	break;

   case TAB_LT_NOEOR:
	sprintf(buf,"Error in Labeled Table \"%s\" at logical record %d.",
                tab->hname,tab->currec);
	sprintf(err_buf,"%s-%s",key,"error");
	c_errmsg(buf,err_buf,NON_FATAL);
	sprintf(buf,"%s %s","Number of fields in label vector do not",
		"correspond with number of fields in the Labeled Table record");
	sprintf(err_buf,"%s-%s",key,"field");
	c_errmsg(buf,err_buf,status);
	break;

   case TAB_LT_SEEK:
	sprintf(buf,"Error seeking to EOF in Labeled Table \"%s\".",tab->hname);
	sprintf(err_buf,"%s-%s",key,"seek");
	c_errmsg(buf,err_buf,status);
	break;

   case TAB_LT_FTYPE:
	sprintf(buf,"Labeled Table \"%s\" file type is invalid.",tab->hname);
	sprintf(err_buf,"%s-%s",key,"ftype");
	c_errmsg(buf,err_buf,status);
	break;

   case TAB_LT_SUBFIL:
	sprintf(buf,"Labeled Table \"%s\" subfile list is invalid.",tab->hname);
	sprintf(err_buf,"%s-%s",key,"subfil");
	c_errmsg(buf,err_buf,status);
	break;

   case TAB_LT_PUTVEC:
	sprintf(buf,"Labeled Table \"%s\" label vector is invalid.",tab->hname);
	sprintf(err_buf,"%s-%s",key,"labvec");
	c_errmsg(buf,err_buf,status);
	break;

   case TAB_ALLOC:
	sprintf(err_buf,"%s-%s",key,"alloc");
	c_errmsg("Error allocating dynamic memory",err_buf,status);
	break;

   case TAB_FORTRAN_MAXFLD:
 	sprintf(buf,"%s%s","Number of columns in the Labeled Table is ",
                "too large for the application program");
	sprintf(err_buf,"%s-%s",key,"col");
	c_errmsg(buf,err_buf,status);

   case TAB_LT_GETDATE:
	sprintf(buf,"Error in Labeled Table \"%s\" at logical record %d.",
                tab->hname,tab->currec);
	sprintf(err_buf,"%s-%s",key,"error");
	c_errmsg(buf,err_buf,NON_FATAL);
	sprintf(err_buf,"%s-%s",key,"readfld");
	c_errmsg("Error reading the date field from the LT",err_buf,
		  status);
	break;

   case TAB_LT_GETTIME:
	sprintf(buf,"Error in Labeled Table \"%s\" at logical record %d.",
                tab->hname,tab->currec);
	sprintf(err_buf,"%s-%s",key,"error");
	c_errmsg(buf,err_buf,NON_FATAL);
	sprintf(err_buf,"%s-%s",key,"readfld");
	c_errmsg("Error reading the time field from the LT",err_buf,
		  status);
	break;

   default:
	sprintf(err_buf,"%s-%s",key,"errcode");
	c_errmsg("Undefined error code in error routine",err_buf,status);
   }

if (strlen(msg))
   c_errmsg(msg,err_buf,severity);

return;
}
