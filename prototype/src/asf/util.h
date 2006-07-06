/**
  C++ utility routines.
  
  This file is #included by plugin.h; don't include it alone.

  by Orion Sky Lawlor, olawlor@acm.org, 2006/05/11 (ASF)
*/
#ifndef __ASF_UTIL_H
#define __ASF_UTIL_H


/*****************************************
FileUtil:
        A collection of file I/O utilities.
Actually implemented in asf/util.cpp

extExists returns whether the given
file basename and extension exist and
are readable.

fileExists returns whether the given file
name exists and is readable.

findExt returns a pointer to the beginning
(the period) of the given name's first
extension, or NULL if none exists.

appendExt allocates its return string
on the heap, so it must be free'd, or
memory will leak.  It can take a NULL extension,
whereupon it just allocates a copy of the given
string, and returns it.

fopenImage first tries to open the given image 
name, then appends ".img" and tries again.
It returns a pointer to the opened file.
*/

/**
 Stick the extension "newExt" onto the end of "name" appropriately.
   @param name  Filename to change the extension of.  For example, "foo.txt".
   @param newExt New filename extension.  For example, ".bar".
   @param return Filename with new extension, like "foo.bar".
*/
ASF_COREDLL std::string appendExt(const char *name,const char *newExt);

/** Return true if fileExists(appendExt(name,newExt)) */
ASF_COREDLL int extExists(const char *name,const char *newExt);
/** Return true if "name" exists and is readable */
ASF_COREDLL int fileExists(const char *name);
/** Return the start of the extension part of "name", or NULL if none exists */
ASF_COREDLL const char *findExt(const char *name);
/** Open an image file for reading or writing. */
ASF_COREDLL FILE *fopenImage(const char *name,const char *accessType);


#endif
