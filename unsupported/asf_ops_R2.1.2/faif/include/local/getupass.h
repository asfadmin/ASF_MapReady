/*==============================================================================
Filename:	getupass.h

Description:
	This header file contains the definition of the FAIF encryption key;
the data, key and ivector buffer sizes; and the name of directory where 
password files are stored.

Creator:	Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:		

SCCS Info:
   %W%
==============================================================================*/

#ifndef _GETUPASS_
#define _GETUPASS_

#define BUFSIZE 1024                /* Size of buffer for data to encrypt */
#define KEYSIZE 8                   /* Encryption key buffer size */
#define IVSIZE  8                   /* IVector buffer size */

#define FAIF_ENC_KEY    "qkg&:#u"   /* Encryption key */
#define FAIF_FTPUPW_DIR "FTP/upw"   /* Password file directory under 
				    -- FAIF rootpath 
				    */

#endif /* _GETUPASS_ */

/* End of File */
