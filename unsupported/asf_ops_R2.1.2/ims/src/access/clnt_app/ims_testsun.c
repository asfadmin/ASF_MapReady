/* ims_testsun.c -- checks to see if server is a sun */
/* exits w/ 0 if not, 1 is so */
/* use -v to display output */
/* By Jeff Cleveland, Computer Sciences Corp. */

#include <X11/Xlib.h>
#include <stdio.h>
#include <string.h>

main( argc,argv )
int argc;
char **argv;

{ Display *display;
  int returncode;

  display=XOpenDisplay( NULL );
  if ( display==NULL ) {
    fprintf( stderr,"Could not open display.\n" );
    exit( -1 );
  }

  if ( argc==2 && strcmp( argv[1],"-v" )==0 ) {
    printf( "Display: %s\n",DisplayString( display ) );
    printf( "Vendor is \"%s\", release is %i\n",
  	   ServerVendor( display ),VendorRelease( display ) );
  }

  if ( strstr( ServerVendor( display ),"Sun" )!=NULL )
    returncode=1;
  else
    returncode=0;

  XCloseDisplay( display );

  if ( argc==2 && strcmp( argv[1],"-v" )==0 ) 
    printf( "Return code: %d\n",returncode );

  return( returncode );
}

