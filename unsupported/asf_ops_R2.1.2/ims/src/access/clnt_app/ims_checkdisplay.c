/* ims_checkdisplay.c -- checks a user's display var for connectability

/* exit w/ return 1: can't connect to display */

#include <X11/Xlib.h>
#include <stdio.h>

main()

{ Display *display;

  display=XOpenDisplay( NULL );

  if ( display==NULL )
    exit( 1 );
  else
    exit( 0 );
}
