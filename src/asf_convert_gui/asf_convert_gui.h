#ifndef __ASF_CONVERT_GUI_H
#define __ASF_CONVERT_GUI_H

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <gtk/gtk.h>
#include <glade/glade.h>

#define JPEG 0
#define PPM 1
#define GEOTIFF 2

/* for win32, need __declspec(dllexport) on all signal handlers */
#if !defined(SIGNAL_CALLBACK)
#  if defined(win32)
#    define SIGNAL_CALLBACK __declspec(dllexport)
#  else
#    define SIGNAL_CALLBACK
#  endif
#endif
 
#endif
