#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		darframe.c

Description:	

External Functions Defined:
	darframe 
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			

==============================================================================*/
#pragma ident	"@(#)darframe.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_framegen/SCCS/s.darframe.c"

#include <stdio.h>
#include <string.h>
#include <ims_query.h>
#include <ims_cmnQuery.h>
#include "framegen.h"


/*==============================================================================
Function:      darframe 

Description:    

Parameters:     

Returns:        

Creator:        Vance A. Heron

Creation Date:  Tue Jul 23 15:15:20 PDT 1996

Notes:		
==============================================================================*/
int darframe(
   FRMGEN *frmgen, 
   int start_frame,
   int end_frame,
   char *dar_string
)
{
int i, frm_id;
char line[80];

   strcpy(dar_string, "OBJECT=IMS_DAR_FRAME\n");
   sprintf(line, "ORDER_ID=%ld\n", frmgen->dar_id);
   strcat(dar_string,line);
   strcat(dar_string,"ITEM_ID=1\n");
   if (frmgen->site_name != NULL) 
   {
      strcat(dar_string,"SITE_NAME=\"");
      strcat(dar_string, frmgen->site_name);
      strcat(dar_string,"\"\n");
   }

   for (i = start_frame; i <= end_frame; i++) {
      frm_id = i % FRAME_REV;
      strcat(dar_string, "GROUP=IMS_FRAME\n");
      strcat(dar_string, "PLATFORM=\"");
      strcat(dar_string, frmgen->platform);
      strcat(dar_string, "\"\n");
      sprintf(line, "REVOLUTION=%ld\n", frmgen->revolution);
      strcat(dar_string,line);
      sprintf(line, "FRAME_ID=%d\n", (i % FRAME_REV)+1 );
      strcat(dar_string,line);
      strcat(dar_string, "END_GROUP=IMS_FRAME\n");
   }

   strcat(dar_string, "END_OBJECT=IMS_DAR_FRAME\n");
   strcat(dar_string, "END\n");
   return (0);
}
