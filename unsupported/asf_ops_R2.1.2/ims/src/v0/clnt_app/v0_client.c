/* File: $RCSfile$
 *
 * Synopsis:
 *
 * Notes:
 *
 */

/*--------------------------------------------------
 | $Log$
 | Revision 1.1  2004/02/03 03:32:54  pdenny
 | Initial revision
 |
 | Revision 1.7  1993/08/31  21:27:54  sylvain
 | the client gets all odl labels from the client-bin/lbls subdirectory.
 | The image handling has been re-written and cleaned up.
 | errno values are cleared before any of the IK routines are called.
 |
 | Revision 1.6  1992/12/21  16:59:39  sylvain
 | removed signal call from stub driver
 |
 | Revision 1.5  1992/11/25  20:52:21  sylvain
 | this client sends a browse request prior to waiting for the integrated
 | browse results and the image
 |
 | Revision 1.4  1992/11/20  19:14:11  sylvain
 | added some code to handle image ingestion as well.
 |
 | Revision 1.3  1992/09/18  12:43:51  sylvain
 | moved the IK_NameSyslog call prior to the IK_Syslog calls (duh!)
 |
 | Revision 1.2  1992/09/02  14:51:07  sylvain
 | removerd unnecessary includes
 |
 | Revision 1.1  1992/08/26  09:56:24  sylvain
 | Initial revision
 |
 +------------------------------------------------*/

/*
** Undefine Posix Source flag because of imcompatibility
** with IK include files.
*/
#undef _POSIX_SOURCE

#include "IK_Network.h"		/* transport layer include file */
#include "odlinter.h"

extern int       sleep(int);

IK_Errno IK_ImsErrno;

/* NOTE : there are 2 other files that may be created if the socket 
 code was compiled with debugging on. These are TxODLTmpFile, is the 
 tmp file used by IK_TxODL for converting the ODL tree to an ODL label. 
 The other file is RxODLTmpFile, which is used to convert the label read into 
 an ODL tree. Both files will be deleted if IK_Comn.c is compiled with the 
 -DNDEBUG switch on.
 */

#ifndef IN_FILE			/* the label that we are sending 
				 to the server */
#  define IN_FILE   "labelread"
#endif

#ifndef OUT_FILE		/* the label that we read from the server */
#   define OUT_FILE  "labelwrote"
#endif

#ifndef SYSLOG_FILE		/* the syslog data store */
#   define SYSLOG_FILE "syslog.log"
#endif

#ifndef NDEBUG
   int DebugLevel = 0;
#endif



void spin();		/* a spinning thing - borrowed from v0 */

/*   a test client driver program */
int main (argc,argv)
 int argc;			/* number of args passed in */
 char *argv[]; 			/* argument list */
{
   int ret_code;		/* various return codes for testing */
   int port;			/* port to transmit over */
   int receive_image = 0;
   IK_BOOLEAN odlfile = IK_FALSE;	/* are we reading the label from a 
					 * a file or are we creating it */
   char server_host[64]; /* a server host to connect to  */
   int sd;			/* the socket to transmit and receive over */
   AGGREGATE TxTree,		/* the label to be transmitted to the server */
             RxTree,		/* the label received from the server */
             message;		/* used to create an ODL tree */
   char errbuf[IK_MAXBUFLEN];	/* used to create error messages */
   FILE *datafile,		/* the file that contains the ODL label to 
				 * be transmitted to the server, in case 
				 * odlfile == IK_TRUE */
        *odlwrotefile,		/* the file that contains the ODL labe that 
				 * was WRITTEN to the server.
				 * when odlfile == IK_TRUE, this and the 
				 * datafile better conain the same stuff*/
        *odlreadfile;		/* the file the contains the label READ from 
				 * the server */
   char *data;			/* used for creating an ODL tree, when 
				 * odlfile == IK_FALSE */
   PARAMETER tmp;
   VALUE_DATA value;

   IK_DSET_LEVEL(9);
   if (argc < 2) 
	{
      fprintf (stderr, "Usage: %s <port-number> [server-host] [test-file] -or-\n\t %s <port-number> <server-host> image\n", argv[0], argv[0]);
      exit(-1);
   } port = atoi (argv[1]);

   if (argc >= 3)		/* where to ? */
       strcpy (server_host, argv[2]);
   else
       gethostname(server_host, MAXHOSTNAMELEN);

   /* open up a syslog file */
   if (IK_NameSyslog (SYSLOG_FILE) < 0) {
      (void) sprintf (errbuf, "unable to open the syslog file, error = %d \
- %s\n", errno, strerror(errno));      
      IK_Syslog (LOG_DEBUG, errbuf);
   }
   
   printf ("starting %s\n", argv[0]);

   if (argc == 4) 
	{
     if (strcmp (argv[3], "image") == 0) 
	  {
			receive_image = 1;
			if ((datafile = fopen ("lbls/browse.odl", "r")) == NULL)  
			{
	   		IK_Syslog (LOG_ERR, "unable to open label file");
	   		exit (-1);
			}
     }
     else 
	  {
       	if ((datafile = fopen (argv[3], "r")) == NULL)  
			{
	  			IK_Syslog (LOG_ERR, "unable to open label file");
	  			exit (-1);
       	}
     }
     odlfile = IK_TRUE;
   }

   if (!receive_image) 
	{
     /* open up the file to write the odl label to that we READ 
      * from the server */
     if ((odlreadfile = fopen (IN_FILE, "w")) == NULL)  
	  {
       IK_Syslog (LOG_ERR, "unable to open label file");
       exit (-1);
     }
     
     /* open up the file to write the label that we are WRITTING to the server 
	  */
     if ((odlwrotefile = fopen (OUT_FILE, "w")) == NULL)  
	  {
       IK_Syslog (LOG_ERR, "unable to open label file");
       exit (-1);
     }
     
   }

   /*  resgister IK_CloseSyslog - may no be necessary since exit () closes 
    * all file descriptors, but it's better to be safe than... */
   atexit (IK_CloseSyslog);

   /* give the client something to save the odl tree to that we tranmit 
    * and received, respectively */
   TxTree = NewAggregate(NULL, KA_GROUP, "root", NULL);
   RxTree = NewAggregate(NULL, KA_GROUP, "root", NULL);

   if (odlfile) 
	{
       if (ReadLabel (datafile, TxTree) == 0) 
		 {
	  		IK_Syslog (LOG_ERR, "unable to parse the label file");
	  		exit (-1);
       }
       (void) fclose (datafile);
    }

   else 			   /* create an odl tree */
  	{		
      if ((message = 
	   NewAggregate (TxTree, KA_GROUP, "Product Result", NULL)) == NULL)
      {
	 		IK_Syslog (LOG_ERR, "unable to create ODL parent aggregate");
	 		exit (-1);
      }
				/*  every msg needs an id. */
      if ((tmp = NewParameter (message, KP_ATTRIBUTE, "MESSAGE_ID")) == NULL)
		{
	 		IK_Syslog (LOG_ERR, "unable to create ODL parameter");
	 		exit (-1);
      }

      tmp->value_kind = KV_SCALAR;
      data = "1";
      value = ODLConvertString (data, strlen (data));
      NewValue (tmp, &value);

      tmp = NewParameter(message, KP_ATTRIBUTE, "Start_Date");
      tmp->value_kind = KV_SCALAR;
      data = "1992/08/11T15:33:33";
      value = ODLConvertDateTime(data, strlen(data));
      NewValue(tmp, &value);
   }

   /* create a socket and connect to the server */
   errno = 0;
   if ((sd = IK_Connect(server_host, port)) < 0) 
	{
      (void) sprintf (errbuf, "Connect failed with errno = %d - %s\n",
	       errno, strerror (errno));
      IK_Syslog (LOG_DEBUG, errbuf);
      exit (-1);
   }
   
   (void) sprintf (errbuf, "****** A connection has been established to the server %s\n", server_host);
   IK_Syslog (LOG_NOTICE, errbuf);
   
   if (receive_image) goto image;

	/* Write lable to the screen, for debugging */
   (void)  WriteLabel (stderr, TxTree);

   (void)  WriteLabel (odlwrotefile, TxTree);

   errno = 0;

   if (IK_TxODL (sd, &TxTree) < 0) 
	{
      printf ( "Transmit failed with errno = %d \n",
			errno);

      (void) sprintf (errbuf, "Transmit failed with errno = %d - %s\n",
	       errno, strerror (errno));
      IK_Syslog (LOG_DEBUG, errbuf);
      exit (-1);
   }

	(void) fprintf (stderr, "sent a chunk, now waiting to receive one\n");
   (void) sprintf (errbuf, "the label has been written to %s, and the file has been rewound\n", OUT_FILE);
   IK_Syslog (LOG_DEBUG, errbuf);
   IK_Syslog (LOG_NOTICE, "**** The client has successfull sent a ODL tree to the server\n");

   /* wait for a reply - the socket is non-blocking, so lets do something 
      usefull if there isn't a message to read */
   errno = 0;
   while ((ret_code = IK_RxODL (sd, &RxTree)) < 0) 
	{
      if (errno == EWOULDBLOCK) 
		{ 
	 		(void) sleep (2);
	 		spin();
	 		errno = 0;
	 		continue;
      }
      else 
		{

	 		(void) sprintf (errbuf, "IK_RxODL failed with errno = %d - %s\n",
			 	errno, strerror (errno));
	 		IK_Syslog (LOG_DEBUG, errbuf);
	 		exit (-1);
      }
   }


   IK_Syslog (LOG_NOTICE, "** A ODL Tree was received from the server\n");

   fprintf (stderr, "the label has been written %s, and the file has been rewound\n",IN_FILE); 

/* jlw */
#	ifdef DEBUG
	printf ("The client has received a label ==>\n");
	WriteLabel (stderr, RxTree);
#	endif

	 WriteLabel (odlreadfile, RxTree); 
   (void) sprintf (errbuf, "the label has been written to %s, and the file has been rewound\n", IN_FILE);
  	IK_Syslog (LOG_DEBUG, errbuf);

   errno = 0;

   if (IK_Shutdown (sd) < 0) 
	{
      fprintf (stderr, "Shutdown failed\n");

      IK_Syslog (LOG_DEBUG, "Shutdown failed");
      exit (-1);
   }

   fprintf (stderr, "> connection has been closed\n");

   IK_Syslog (LOG_NOTICE, "> The connection has been closed\n");
   exit (0);

 image:
   {
      FILE *quit_fp;
      int image_size, size;
      unsigned short chunk_size;
      VALUE image_id;
      char buffer[IK_NETBLK_SZ];
      AGGREGATE QuitTree;
      FILE *fp;
      
      if ((fp = fopen ("image", "w")) == NULL) 
		{
	 		fprintf (stderr, "couldn't open the file to put the image in\n");
	 		exit(-1);
      }
      
      if ((quit_fp = fopen ("../server-bin/quit.odl", "r")) == NULL) 
		{
	 		fprintf (stderr, "couldn't open the QUIT label file\n");
	 		exit(-1);
      }

      QuitTree =NewAggregate (NULL, KA_GROUP, "root", NULL);
      
      (void) ReadLabel (quit_fp, QuitTree);
		
      if (IK_TxODL (sd, &TxTree) < 0) 
		{
	 		(void) sprintf (errbuf, "Transmit failed with errno = %d - %s\n",
			 	errno, strerror (errno));
	 		IK_Syslog (LOG_DEBUG, errbuf);
	 		exit (-1);
      }
      
      /* get the integrated_browse_result tree */
      errno = 0;
      while ((ret_code = IK_RxODL (sd, &RxTree)) < 0) 
		{
	 		if (errno == EWOULDBLOCK) 
			{
			
	    		(void) sleep (2);
	    		errno = 0;
	    		spin();
	    		continue;
	 		}
	 		else 
			{
	    		(void) sprintf (errbuf, "IK_RxODL failed with errno = %d - %s\n",
			   	 errno, strerror (errno));
	    		IK_Syslog (LOG_DEBUG, errbuf);
	    		exit (-1);
	 		}
      }  /* while ret_code of IK-RxODL <0 */

      /* get the "image_size" */
      
      if ((image_id = 
	   		FirstValue(FindParameter (FindGroup (FindGroup (RxTree,
							   "INTEGRATED_BROWSE_RESULT"), "IMAGE"), "IMAGE_SIZE"))) 
	  			== NULL) 
      {
	  		fprintf (stderr, "unable to find the IMAGE_SIZE for image ingestion\n");
	  		exit (-1);
      }
		 
      image_size = atoi (image_id->item.value.string);

      IK_vSyslog (LOG_ERR, "The image is going to be %d bytes long", image_size);

      /* read in the image */
      while (image_size > 0) 
		{
	 		if (image_size > IK_NETBLK_SZ) 
			{
	    		chunk_size = (unsigned short) IK_NETBLK_SZ;
	    		image_size -= IK_NETBLK_SZ;
	 		}
	 		else 
			{
	    		chunk_size = (unsigned short) image_size;
	    		image_size = 0;
	 		}
	 
	 		errno = 0;

	 		size = IK_GetImage (sd, buffer, chunk_size);
		
			if (size < 0 && errno == EWOULDBLOCK) 
			{ 
	    		(void) sleep (2);
	    		spin();
	    		continue;
	 		}       
			else if (size  < 0) 
			{
	    		printf ( "IK_GetImage failed with errno = %d \n",
			  	  errno);

	    		(void) sprintf (errbuf, "IK_GetImage failed with errno = %d - %s\n",
			   	 errno, strerror (errno));
	    		IK_Syslog (LOG_DEBUG, errbuf);
	    		exit (-1);
	 		}
	 
#ifdef ABORT_ON_IMAGE
	 		if (IK_TxODL (sd, &QuitTree) < 0) 
			{
	    		printf ("Transmit failed with errno = %d \n",
			  	  errno);
				 
	    		(void) sprintf (errbuf, "Transmit failed with errno = %d - %s\n",
			  	  errno, strerror (errno));
	    		IK_Syslog (LOG_DEBUG, errbuf);
	    		(void) IK_Shutdown (sd);
	    		exit (-1);
	 		}     
#endif

	 		if (fwrite (buffer, (size_t) sizeof (char), (size_t) size, fp) < 0) 
			{
	    		fprintf (stderr, "unable to write the image to the 'image' file, \
					errno = %d - %s\n", errno, strerror (errno));

	    		exit (-1);
	 		} 

	 		printf ("IK_GetImage received %u bytes of the image", size);

	 		IK_vSyslog (LOG_DEBUG, "IK_GetImage received %u bytes of the image", 
		   	  size);
      } /* while (image_size >0) */

      if (size >= 0)
	  		IK_Syslog (LOG_DEBUG, "The image has been read");
			
	  		 printf ("The image has been read\n");
	
   } /* image: */

   exit (0);

} /* main */
    
void
spin()
{
    static unsigned char wait_loops = 0;	/* number of loops waiting
						 * for message */

    /* While waiting for server, display a spinner */
    static char     stroke[] = {'|', '\\', '-', '/'};

    (void) fprintf(stderr, "%c\b", stroke[wait_loops++ % 4]);
 }

/*--QED--*/
