/*========================================================================
 *
 * Name - config.h
 *
 * Version:	1.11
 *
 * ccsid:	@(#)config.h	1.11 - 06/28/93 09:13:43
 * from: 	ccs/s.config.h
 * date: 	06/28/93 09:14:48
 *
 * Copyright (c) 1991-93 Bruce Schuchardt.
 * Read the file cpyright.h for full copyright information.
 *
 *
 * Description:
 *   configuration options for xgrabsc
 *
 *========================================================================
 */

/* if you do not have memcpy routines but do have bcopy, define the following */
/* #define BCOPY */

/* if you have neither memcpy nor bcopy, or for some other reason would like
 * to use xgrabsc's version of memcpy, define the following */
/* #define MEMCPY */

/* if you want to use some other key than Control for selection, define
 * the following and replace ControlMask with the mask of your choice */
/* #define SELECTION_MASK ControlMask */

/* when forming postscript output and the "-c" option has not been specified,
 * xgrabsc will make a pass over the image to determine whether run-length
 * encoding will shrink the output appreciably.  If you are building xgrabsc
 * for a slower machine, you may wish to avoid this overhead.  If so,
 * define the following */
/* #define NO_RLE_CHECKS */

/* When forming color postscript output, xgrabsc will include a color to
 * greyscale converter in the output so that it can also be printed on
 * non-color printers.  If you prefer to have this converter omitted from
 * color postscript output by default, change the following to FALSE */
#define DEFAULT_NEED_COLORIMAGE_PROC  TRUE


/* Xgrabsc will look for a virtual-window manager's root window for
 * window dumps.  If the virtual-window manager support code gives you
 * trouble, define the following to omit it */
/* #define NO_VIRTUAL_WINDOW */

/* Xgrabsc can write code to check printer memory availability before
 * attempting to display an image.  Postscript previewers may cause problems
 * with this since they usually allocate memory as needed, and so will
 * often fail memory-availability checks.
 * Editors, such as FrameMaker can also cause problems like this, so
 * the checks are always turned off when Encapsulated Postscript format is
 * requested.
 * A command line switch can also turn checks on or off, but they can be
 * disabled totally here by commenting out the following line. */
#define NO_PRINTER_MEMORY_CHECKS



/* The "Encapsulated PostScript FILES Specification Version 2.0" document
 * specifies that preview images in EPSF files must use 1 for black and
 * 0 for white.  FrameMaker adheres to this but xfig does not.  The default
 * for xgrabsc is to follow the specification, but if you use a tool like
 * xfig that expects 0 to be black, change the following definition.
 */
#define EPSF_BLACK 1

/* xgrabsc has a hard-coded notion of the size of your printer's paper.
 * The default is for US Letter size (8.5x11.0 inch).
 * The inset sizes control the maximum size of the image on the page.
 * These are preset to leave at least a 0.5 inch border around the image.
 */
#define PAPER_WIDTH         8.5   /* inches are assumed */
#define PAPER_HEIGHT       11.0

#define VERT_MARGIN	    0.5
#define HORIZ_MARGIN	    0.5


/* xgrabsc generates images by bunches of IMAGE_CODEWIDTH chars for the
 * images, and PREVIEW_CODEWITH chars for the EPSI preview image.
 * Change them if you want larger or smaller lines, but make sure the
 * numbers you choose are divisible by 2!
 */
#define IMAGE_CODEWIDTH	     60
#define PREVIEW_CODEWIDTH    60


/* xgrab can add/increment numbers in file names for 'to file' output.
 * If there is no number, a '1' will be added after the last period in
 * the file name or at the end of the file name if there is no period.
 * Numbers found in such positions will be bumped after each grab.
 */
#define BUMP_FILENAMES


/* the following options let you specify what the default file name
 * and command are for the xgrab program, and the default pre and post
 * snapshot sleep times
 */
#define DEFAULT_FILENAME  screen1.dmp
#define DEFAULT_COMMAND   lpr
#define DEFAULT_PRESLEEP  3
#define DEFAULT_POSTSLEEP 0

