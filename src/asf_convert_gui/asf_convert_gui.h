#ifndef __ASF_CONVERT_GUI_H
#define __ASF_CONVERT_GUI_H

#define _GNU_SOURCE
#define USE_GTK_22

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <assert.h>
#include <gtk/gtk.h>
#include <glade/glade.h>
#include <glib.h>
#include <glib/gprintf.h>

#if ( GTK_MAJOR_VERSION >= 2 && GTK_MINOR_VERSION >= 4 \
      && GDK_PIXBUF_MAJOR >= 2 && GDK_PIXBUF_MINOR >= 4 )
#  define THUMBNAILS
#  define THUMB_SIZE 48
#endif

#if ( GTK_MAJOR_VERSION >= 2 && GTK_MINOR_VERSION >= 6 )
#  define FILE_CHOOSER_AVAILABLE
#endif

enum OutputFormat
{
    OUTPUT_FORMAT_JPEG = 0,
    OUTPUT_FORMAT_PPM = 1,
    OUTPUT_FORMAT_TIFF = 2,
    OUTPUT_FORMAT_GEOTIFF = 3,
    OUTPUT_FORMAT_ASF_INTERNAL = 4,
    OUTPUT_FORMAT_CEOS = 5
};

enum InputFormat
{
    INPUT_FORMAT_CEOS_LEVEL0 = 0,
    INPUT_FORMAT_CEOS_LEVEL1 = 1,
    INPUT_FORMAT_STF = 2,
    INPUT_FORMAT_COMPLEX = 3,
    INPUT_FORMAT_ESRI = 4,
    INPUT_FORMAT_ENVI = 5,
    INPUT_FORMAT_ASF_INTERNAL = 6
};

enum InputType
{
    INPUT_TYPE_SIGMA = 0,
    INPUT_TYPE_BETA = 1,
    INPUT_TYPE_GAMMA = 2,
    INPUT_TYPE_AMP = 3,
    INPUT_TYPE_POWER = 4
};

enum ScalingMethod
{
    SCALING_METHOD_SIGMA = 0,
    SCALING_METHOD_MINMAX = 1,
    SCALING_METHOD_TRUNCATE = 2
};
    
enum ProjectionOptions
{
  PROJ_UTM = 0,
  PROJ_PS = 1,
  PROJ_ALBERS = 2,
  PROJ_LAMAZ = 3,
  PROJ_LAMCC = 4 // currently unavailable through GUI
};

enum Datums
{
    DATUM_WGS84 = 0,
    DATUM_NAD27 = 1,
    DATUM_NAD83 = 2
};

enum
{
  RESAMPLE_NEAREST_NEIGHBOR = 0,
  RESAMPLE_BILINEAR = 1,
  RESAMPLE_BICUBIC = 2
};
  

/* for win32, need __declspec(dllexport) on all signal handlers */
#if !defined(SIGNAL_CALLBACK)
#  if defined(win32)
#    define SIGNAL_CALLBACK __declspec(dllexport)
#  else
#    define SIGNAL_CALLBACK
#  endif
#endif

#include "settings.h"
#include "find_in_path.h"
#include "execute.h"
#include "callbacks.h"
#include "utility.h"
#include "dnd.h"
#include "popup_menu.h"
#include "file_list.h"
#include "help.h"
#include "rename_output.h"
#include "file_selection.h"
#include "state.h"
#include "naming_scheme.h"
#include "metadata.h"
#include "summary.h"
#include "geocode.h"
#include "win_font.h"
#include "projfile.h"
#include "view_output.h"

extern const char DIR_SEPARATOR;
extern const char PATH_SEPATATOR;

/* these are our global variables ... */

/* xml version of the .glade file */
extern GladeXML *glade_xml;

/* The files listing, and an associated lock that is needed because
   the thumbnail loader threads look in here to figure out which files
   thumbnails they should be loading into which treeview rows.  */
extern GStaticRecMutex list_store_lock;
/* Convenience macros for locking the list_store.  */
#define LSL g_static_rec_mutex_lock (&list_store_lock)
#define LSU g_static_rec_mutex_unlock (&list_store_lock)
extern GtkListStore *list_store;

/* TRUE during processing */
extern gboolean processing;

/* The settings when the user clicked "Execute" (or, "Load") */
extern Settings * settings_on_execute;

/* where should we put the generated files */
extern gchar * output_directory;

/* current naming scheme */
extern NamingScheme * current_naming_scheme;

/* are thumbnails supported ? */
extern gboolean use_thumbnails;
#endif
