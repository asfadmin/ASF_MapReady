#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		get_table_index.c

Description:	

External Functions Defined:
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			

==============================================================================*/
#pragma ident	"@(#)get_table_index.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/mapr/SCCS/s.get_table_index.c"

#include "table_def.h"

int	
get_table_index( table, code )
struct	table	*table;
char	*code;
{
	int	index;

	if (!table || !code)
		return -1;
	for (index = 0 ; table[index].code ; index++)
	{
		if (! strcmp(code, table[index].code))
			return index;
	}
	return -1;
}

void get_table_desc( table, index, desc, desc_len )
struct  table   *table;
int     *index;
char    *desc;
int     *desc_len;
{
	if (!table || (*index<0))
		return ;
	strncpy(desc, table[*index].desc, *desc_len - 1);
}
