/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* best_val.c -- determine the best guess value of a parameter
		given a set of possible values */

#include <stdio.h>
#include <procfil.h>
#include <procdec.h>


#define TBSIZE 20

typedef struct {    /* value table structure */
	int size;	/* number of entries in table */
	int used;	/* number of entries used so far */
	int val[1];	/* list of values */
} VTAB, *VTAB_PTR;

static VTAB_PTR bitab[TBSIZE];	/* pointers to value tables */



/* init_best_val(entries) ----------------------------------------------
	This routine initializes a best value table of the needed size
	for the given number of entries, and returns an integer id
	representing the table assigned.
*/

init_best_val(entries)
	int entries;
{
	int i;
	static int first = 1;
	char *malloc();

	if (first) {
	    first = 0;
	    for (i = 0; i < TBSIZE; i++)
		bitab[i] = NULL;
	}
	for (i = 0; i < TBSIZE; i++) {
	    if (bitab[i] != NULL)
		continue;
	    if ((bitab[i] = (VTAB_PTR) malloc(sizeof(VTAB)
			+ entries * sizeof(int))) == NULL) {
		printf("Out of memory in init_best_val\n");
		printf("  (asked for %d entries)\n",entries);
		exit (1);
	    }
	    bitab[i]->size = entries;
	    bitab[i]->used = 0;
	    return (i);
	}
	printf("Out of table space in init_best_val\n");
	exit (1);
}


/* save_best_val(id,value) ---------------------------------------------
	This routine adds the given value to the given best value table.
*/

save_best_val(id,value)
	int id, value;
{
	VTAB_PTR vp;

	if (id < 0 || id >= TBSIZE || (vp = bitab[id]) == NULL) {
	    printf ("Invalid or unused id in save_best_val\n");
	    return;
	}
	if (vp->used >= vp->size) {
	    printf ("Too many entries in save_best_val\n");
	    return;
	}
	vp->val[vp->used++] = value;
}


/* save_best_seq_val(id,value,mod) -------------------------------------
	This routine adds the given value to the given best value table.
	The value is supposed to be consecutively incrementing by 1.
	If mod > 0, the value is modulo that number.
*/

save_best_seq_val(id,value,mod)
	int id,value,mod;
{
	VTAB_PTR vp;
	int i;

	if (id < 0 || id >= TBSIZE || (vp = bitab[id]) == NULL) {
	    printf ("Invalid or unused id in save_best_seq_val\n");
	    return;
	}
	if (vp->used >= vp->size) {
	    printf ("Too many entries in save_best_seq_val\n");
	    return;
	}
	i = vp->used;
	vp->val[i] = value - i;
	if (vp->val[i] < 0 && mod > 0)
	    vp->val[i] += mod;
	vp->used++;
}


/* get_best_val(id) ----------------------------------------------------
	This routine returns the best guess value of the parameter
	represented by id, and clears out the value table.  If there
	is no best guess, or if there is a specification error, the
	routine returns FAIL.  Since FAIL is a valid integer value (-1),
	that value should not be one of the acceptable values for the
	best guess.  "Best guess" means that more than half the input
	entries had that value.
*/

get_best_val(id)
	int id;
{
	VTAB_PTR vp;
	int i,n,index,val,half;

	if (id < 0 || id >= TBSIZE || (vp = bitab[id]) == NULL) {
	    printf ("Invalid or unused id in get_best_val\n");
	    return (FAIL);
	}
	
	bitab[id] = NULL;
	if (vp->used <= 0) {
	    free (vp);
	    return (FAIL);
	}
	half = (vp->used / 2) + 1;
	index = 0;
	while (index >= 0 && index < half) {
	    val = vp->val[index];
	    i = index;
	    index = -1;
	    n = 0;
	    while (i < vp->used) {
		if (vp->val[i++] == val) {
		    if (++n >= half) {
			free (vp);
			return (val);
		    }
		}
		else if (index < 0)
		    index = i-1;
	    }  /* while i */
	}  /* while index */
	free (vp);
	return (FAIL);
}
