/*=============================================================================
 *  @(#)mem.h	1.3 95/02/03 14:53:02
 *
 *  Memory Management Library.
 *  Alaska SAR Facility (ASF) Project.
 *
 *  Copyright (C) Jet Propulsion Laboratory.
 *
 *============================================================================*/

#ifndef _MEM_H
#define _MEM_H

static char sccsid_mem_h[] =
	"@(#)mem.h	1.3 95/02/03 14:53:02";

#include <stdlib.h>

void  *m_alloc(size_t size, void *region);
void   m_free(void *region);
void   m_free_all(void);
void  *m_root();
void  *m_parent();

#endif /* !_MEM_H */
