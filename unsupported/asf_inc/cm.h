/*****************************************************************************
 *									     *
 *	This is the include file for the 'cm' routines.			     *
 *									     *
 *	Modified by D. Akkerman, USGS/EROS Data Center, 5/90.		     *
 *	 - Added the TAE_UPATH define for changes to the cmhost and	     *
 *	   cmtae routines.						     *
 *									     *
 *	Modified by B. Davis, USGS/EROS Data Center, 9/90.		     *
 *	 - added the cm tape structures and defines for cmstor/cmrestor.     *
 *									     *
 *	Modified by D. Etrheim, USGS/EROS Data Center, 11/90.		     *
 *	 - added online/offline/both defines                                 *
 *****************************************************************************/

#define TAE_EXT  ';'
#define HOST_EXT '.'
#define TAE_SEP  '.'
#define HOST_SEP '_'

#define TAE_DIR_BEG '['
#define TAE_DIR_SEP '.'
#define TAE_DIR_END ']'
#define TAE_PARENT  '-'
#define TAE_UPATH   '%'
#ifdef vms
#define HOST_DIR_BEG '['
#define HOST_DIR_SEP '.'
#define HOST_DIR_END ']'
#define HOST_PARENT  '-'
#else
#define HOST_DIR_BEG '/'
#define HOST_DIR_SEP '/'
#define HOST_DIR_END '/'
#define HOST_PARENT  '.'
#endif

#define ONLINE 1
#define OFFLINE 2
#define BOTH 3

/*
   for Catalog Manager tapes
*/
struct TAPELIST
	{
	char	filestatus;		/* status of the file to stor   */
					/* blank - stored on tape	*/
					/* D	 - marked for Deletion	*/
					/* H	 - Hidden		*/
	char	lassys[11];		/* system name from syschar 	*/
	char	data_type[4];		/* code for image data type	*/
	char	lines[5];		/* number of image lines	*/
	char	samples[5];		/* number of image samples	*/
	char	bands[3];		/* number of image bands	*/
	char	tape_id[6];		/* name of stor tape		*/
	char	timestamp[11];		/* file create date YYDDDHHMMSS */
	char	feet[9];		/* approx feet written by file  */
	char	totft[9];		/* total feet utilized this tape*/
	char	bpi[5];			/* tape density			*/
	char	storetime[11];		/* date and time the file 	*/
					/* described by this record was */
					/* stored - YYDDDHHMMSS format	*/
	char	file_no[4];		/* file no. in this tapes sequence*/
	char	tapeloc;		/* physical tape location code  */
	char	end_space;		/* for null-terminated raw i/o	*/
	} ;

struct TAPENAME
	{
	char	filestatus;		/* T - denotes this record is a	*/
					/*     tape name		*/
	char	loc_chain[2];		/* location in tape chain 	*/
					/*   "HD" - head tape	  	*/
					/*   "CO" - continuation tape	*/
	char	tape_no[2];		/* number of tape in the chain	*/
	char	back_chain[6];		/* tape chain back pointer:	*/
					/* previous tape label		*/
	char	blk_count[6];		/* number of blocks written	*/
	char	space1[12];		/* blank filled 		*/
	char	tape_id[6];		/* name of stor tape		*/
	char	space2[11];		/* blank filled 		*/
	char	feet[9];		/* approx feet written on tape  */
	char	totft[9];		/* total feet on this tape	*/
	char	bpi[5];			/* tape density			*/
	char	timestamp[11];		/* tape init date YYDDDHHMMSS 	*/
	char	file_no[4];		/* files on this tape 		*/
	char	tapeloc;		/* physical tape location code  */
	char	end_space;		/* for null-terminated raw i/o	*/
	} ;


#define COMPUTER_ROOM '0'		/* single digit character code 	*/
					/* for physical location of tape*/
#define TAPE_LIBRARY '1'		/* single digit character code 	*/
					/* for physical location of tape*/


#define TPLSLEN 86			/* each field of TAPELIST is one
					  greater in length than required
					  for the data it holds in order
					  to allow it to contain a null-
					  terminated string		*/

#define MAXFILES 100			/* maximum number of files to be
					   associated with and image,
					   in order to help limit the
					   size of array declarations.	*/

#define DUPE 2				/* return status of a tape dir-
					   ectory call, meaning the file
					   already exists on tape, or in
					   the catalog.  Sometimes this
					   is desired, sometimes not	*/

#define MAXTPWT 32768			/* maximum number of bytes that
					   can be streamed together and
					   read/written to/from a single
					   buffer by a single read/write.
					   This value is the smallest one
					   of the current systems,
					   sun-bsd.  If LAS is ported to
					   a new different machine with a
					   smaller MAX, this value will
					   need to be adjusted.		*/

#define TPIDLEN 7			/* to help limit the size of 2d   
					   arrays by not using CMLEN.
					   corresopnds to TAPELIST.tape_id,
					   with an extra char for
					   null-terminated strings.	
					   there became occasions when a
					   tapeid variable was needed in
					   a function in which there were
					   no structures declared that 
					   knew the size of tape_id.	*/
