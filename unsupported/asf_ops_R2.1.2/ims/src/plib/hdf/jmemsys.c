/*
 * jmemsys.c
 *
 * Copyright (C) 1992, Thomas G. Lane.
 * This file is part of the Independent JPEG Group's software.
 * For conditions of distribution and use, see the accompanying README file.
 *
 * This file chooses between memory managers, based on #define's in hdfi.h
 *
 */

#include "jinclude.h"

/* Check for a machine which doesn't need our backing store manager */
#if JMEMSYS == MEM_NOBS
#include "jmemnobs.c"

/* Check for a machine in which we can use our ANSI-compatible backing store */
#elif JMEMSYS == MEM_ANSI
#include "jmemansi.c"

/* Check for a machine with a non-ANSI backing store */
#elif JMEMSYS == MEM_NAME
#include "jmemname.c"

/* Check for a MS-DOS machine */
#elif JMEMSYS == MEM_DOS
#include "jmemdos.c"

/* Error, a memory manager hasn't been defined */
#else
/* #error A memory manager has not been defined. */

#endif

