/*
 * jmemansi.c  (jmemsys.c)
 *
 * Copyright (C) 1992, Thomas G. Lane.
 * This file is part of the Independent JPEG Group's software.
 * For conditions of distribution and use, see the accompanying README file.
 *
 * This file provides a simple generic implementation of the system-
 * dependent portion of the JPEG memory manager.  This implementation
 * assumes that you have the ANSI-standard library routine tmpfile().
 * Also, the problem of determining the amount of memory available
 * is shoved onto the user.
 */

#include "jinclude.h"
#include "jmemsys.h"

#ifdef INCLUDES_ARE_ANSI
#include <stdlib.h>		/* to declare malloc(), free() */
#else
extern VOID * malloc PROTO((size_t size));
extern VOID free PROTO((VOID *ptr));
#endif

#ifndef SEEK_SET		/* pre-ANSI systems may not define this; */
#define SEEK_SET  0		/* if not, assume 0 is correct */
#endif


static external_methods_ptr methods; /* saved for access to error_exit */

static long total_used;		/* total memory requested so far */


/*
 * Memory allocation and freeing are controlled by the regular library
 * routines malloc() and free().
 */

GLOBAL VOIDP
#ifdef PROTOTYPE
jget_small (size_t sizeofobject)
#else
jget_small (sizeofobject)
size_t sizeofobject;
#endif
{
  total_used += sizeofobject;
  return (VOID *) malloc(sizeofobject);
}

GLOBAL VOID
#ifdef PROTOTYPE
jfree_small (VOIDP object)
#else
jfree_small (object)
VOIDP object;
#endif
{
  free(object);
}

/*
 * We assume NEED_FAR_POINTERS is not defined and so the separate entry points
 * jget_large, jfree_large are not needed.
 */


/*
 * This routine computes the total memory space available for allocation.
 * It's impossible to do this in a portable way; our current solution is
 * to make the user tell us (with a default value set at compile time).
 * If you can actually get the available space, it's a good idea to subtract
 * a slop factor of 5% or so.
 */

#ifndef DEFAULT_MAX_MEM		/* so can override from makefile */
#define DEFAULT_MAX_MEM		1000000L /* default: one megabyte */
#endif

GLOBAL long
#ifdef PROTOTYPE
jmem_available (long min_bytes_needed, long max_bytes_needed)
#else
jmem_available (min_bytes_needed, max_bytes_needed)
long min_bytes_needed;
long max_bytes_needed;
#endif
{
  return methods->max_memory_to_use - total_used;
}


/*
 * Backing store (temporary file) management.
 * Backing store objects are only used when the value returned by
 * jmem_available is less than the total space needed.  You can dispense
 * with these routines if you have plenty of virtual memory; see jmemnobs.c.
 */


METHODDEF VOID
#ifdef PROTOTYPE
read_backing_store (backing_store_ptr info, VOIDP buffer_address,
		    long file_offset, long byte_count)
#else
read_backing_store (info, buffer_address, file_offset, byte_count)
backing_store_ptr info;
VOIDP buffer_address;
long file_offset;
long byte_count;
#endif
{
  if (fseek(info->temp_file, file_offset, SEEK_SET))
    ERREXIT(methods, "fseek failed on temporary file");
  if (JFREAD(info->temp_file, buffer_address, byte_count)
      != (size_t) byte_count)
    ERREXIT(methods, "fread failed on temporary file");
}


METHODDEF VOID
#ifdef PROTOTYPE
write_backing_store (backing_store_ptr info, VOIDP buffer_address,
		     long file_offset, long byte_count)
#else
write_backing_store (info, buffer_address, file_offset, byte_count)
backing_store_ptr info;
VOIDP buffer_address;
long file_offset;
long byte_count;
#endif
{
  if (fseek(info->temp_file, file_offset, SEEK_SET))
    ERREXIT(methods, "fseek failed on temporary file");
  if (JFWRITE(info->temp_file, buffer_address, byte_count)
      != (size_t) byte_count)
    ERREXIT(methods, "fwrite failed on temporary file --- out of disk space?");
}


METHODDEF VOID
#ifdef PROTOTYPE
close_backing_store (backing_store_ptr info)
#else
close_backing_store (info)
backing_store_ptr info;
#endif
{
  fclose(info->temp_file);
  /* Since this implementation uses tmpfile() to create the file,
   * no explicit file deletion is needed.
   */
}


/*
 * Initial opening of a backing-store object.
 *
 * This version uses tmpfile(), which constructs a suitable file name
 * behind the scenes.  We don't have to use temp_name[] at all;
 * indeed, we can't even find out the actual name of the temp file.
 */

GLOBAL VOID
#ifdef PROTOTYPE
jopen_backing_store (backing_store_ptr info, long total_bytes_needed)
#else
jopen_backing_store (info, total_bytes_needed)
backing_store_ptr info;
long total_bytes_needed;
#endif
{
  if ((info->temp_file = tmpfile()) == NULL)
    ERREXIT(methods, "Failed to create temporary file");
  info->read_backing_store = read_backing_store;
  info->write_backing_store = write_backing_store;
  info->close_backing_store = close_backing_store;
}


/*
 * These routines take care of any system-dependent initialization and
 * cleanup required.  Keep in mind that jmem_term may be called more than
 * once.
 */

GLOBAL VOID
#ifdef PROTOTYPE
jmem_init (external_methods_ptr emethods)
#else
jmem_init (emethods)
external_methods_ptr emethods;
#endif
{
  methods = emethods;		/* save struct addr for error exit access */
  emethods->max_memory_to_use = DEFAULT_MAX_MEM;
  total_used = 0;
}

GLOBAL VOID
#if defined ( PROTOTYPE ) && ! defined ( CONVEX )
jmem_term (VOID)
#else
jmem_term ()
#endif
{
  /* no work */
}
