#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	aps_encrypt.h

Description:	APS username and password encryption header file	

Creator:	Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:		

==============================================================================*/
#pragma ident	"@(#)aps_encrypt.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/include/local/SCCS/s.aps_encrypt.h"


#ifndef _APSENCRYPT_
#define _APSENCRYPT_

#define BUFSIZE 1024    /* Size of buffer for data to encrypt */
#define KEYSIZE 8       /* Encryption key buffer size */
#define IVSIZE  8       /* IVector buffer size */

#define APS_ENC_KEY      "k*&^sn("
#define APS_ACS_UPW_FILE "APS_ACS.upw"
#define APS_IMS_DB_UPW_FILE "APS_IMS_DB.upw"

/* use this routine to obtain the password.  */
char *get_APS_upass(char *upassfile) ;

#endif /* _APSENCRYPT_ */


/* End of File */
