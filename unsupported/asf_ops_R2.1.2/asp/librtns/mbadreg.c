/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* mbadreg.c -- non-graphical register display and set routines

	This set of routines provides display support for the
	hardware.dat data base, without requiring a graphics
	screen.
*/

#include <stdio.h>
#include <ctype.h>
#include <aspdecl.h>
#include <ibobdefs.h>

extern max_cname,max_bname;		/* max size of cage, board names */
extern char system_name[50];		/* name of hardware system */
extern CAGEPTR cage_list[20];
extern int ncages;			/* number of cages defined */
extern int lowcase_sw;			/* 1 = lower case input is OK */
extern char regs_file[50];		/* register load file name */
extern int regs_modified;		/* 1 = modified since loaded */
typedef enum {previous, init, loadfile} rsource;
extern rsource regs_source;		/* register contents source */
static char *rs_name[3] = { "previous data",
			    "initialization values",
			    regs_file };

extern char hw_tbl_name[80];	/* name of hardware data file */

/**********************************************************************/

/* dreg(boardname,regnum) -------------------------------------------
	This routine displays the given register's fields.  If
	boardname is missing, regnum is assumed to be a register
	hardware address.
*/
dreg(boardname,regnum)
char *boardname;
int regnum;
{
	CAGEPTR cp;
	BOARDPTR bp;
	REGPTR rp;
	FIELDPTR fp;
	int f,i,j,rval;
	char t[30];

	get_hardware_table();
	if (boardname[0] == '\0') {
	    find_reg(regnum,&bp,&rp);
	    if (rp == NULL) return;
	} else {
	    find_boardname(boardname,&bp);
	    if (bp == NULL) return;
	    if (regnum > bp->nregs-1) {
		printf("register number too big for this board\n");
		return;
	    }
	}  /* else */
	for (rp = bp->reg_list; rp != NULL; rp = rp->nxt_reg){
	    if( regnum==-1 || regnum == rp->num){
		rval = get_reg(bp,rp);
		if (bp->byteregs) printf("REGISTER %d = %.2X\n",rp->num,rval);
		else printf("%s REGISTER [%3x] %d=%.4X\n",
			bp->name, bp->addr+(rp->num<<1), rp->num,rval);
		i = 0;
		for (fp = rp->field_list; fp != NULL; fp = fp->nxt_field) {
		    f = (rval & fp->mask) >> fp->shift;
		    printf("  %X: %-17s %4X\n",i++,fp->name,f);
		}  /* for fp */
	    }
	}
}

/* sreg(boardname,regnum,fieldnum,value) -------------------------------
	This routine loads a new value into the register or field
	specified by regnum,fieldnum.  If fieldnum is -1, the whole
	register is loaded with the value; otherwise just the given
	field is loaded.
*/
sreg(boardname,regnum,fieldnum,value)
char *boardname;
int regnum,fieldnum,value;
{
	BOARDPTR bp;
	REGPTR rp;
	FIELDPTR fp;
	int i;

	get_hardware_table();
	find_boardname(boardname,&bp);
	if (bp == NULL) return;
	if (regnum > bp->nregs-1) {
	    printf("register number too big for this board\n");
	    return;
	}
	for (rp = bp->reg_list; rp != NULL && regnum != rp->num;
		rp = rp->nxt_reg);
	if (fieldnum < 0) set_reg(bp,rp,value);
	else {
	    if (fieldnum > rp->nfields-1) {
		printf("field number too big for this register\n");
		return;
	    }
	    i = 0;
	    for (fp = rp->field_list; fp != NULL && i++ < fieldnum;
		    fp = fp->nxt_field);
	    if (fp->inverted) value = ~value;
	    set_field(bp,rp,fp,value);
	}
}

/* dc(cagename) -----------------------------------------------------
	This routine displays all the boards and registers in this
	cage.
*/
dc(cagename)
char *cagename;
{
	int i,j,rval;
	CAGEPTR cp;
	BOARDPTR bp;
	REGPTR rp;
	char t[30],*s;

	get_hardware_table();
	if (cagename[0] == '\0') {
	    for (i = 0; i < ncages; i++) {
		cp = cage_list[i];
		printf("%s\n",cp->name);
	    }
	    return;
	}
	s = t;
	while (*s++ = toupper(*cagename++));
	for (i = 0; i < ncages; i++) {
	    cp = cage_list[i];
	    if (strcmp(t,cp->name) == 0) {
		for (j = 0; j < cp->nboards; j++) {
		    bp = cp->board_list[j];
		    printf("%-10s",bp->name);
		    for (rp = bp->reg_list; rp != NULL; rp = rp->nxt_reg) {
			rval = get_reg(bp,rp);
			if (bp->byteregs) printf(" %.2X",rval);
			else printf(" %.4X",rval);
		    }  /* for rp */
		    printf("\n");
		}  /* for j */
		return;
	    }  /* if strcmp */
	}  /* for i */
	printf("cannot find cage name %s in hardware.dat file\n",t);
}

/* find_reg(regaddr,bpout,rpout) ---------------------------------------
	This routine finds the board and register block associated
	with the register whose address is given by regaddr.
*/
find_reg(regaddr,bpout,rpout)
int regaddr;
BOARDPTR *bpout;
REGPTR *rpout;
{
	int i,j;
	CAGEPTR cp;
	BOARDPTR bp;
	REGPTR rp;

	for (i = 0; i < ncages; i++) {
	    cp = cage_list[i];
	    for (j = 0; j < cp->nboards; j++) {
		bp = cp->board_list[j];
		for (rp = bp->reg_list; rp != NULL; rp = rp->nxt_reg) {
		    if (regaddr == rp->addr) {
			*bpout = bp;
			*rpout = rp;
			return;
		    }
		}
	    }
	}
	printf("cannot find register data in hardware.dat\n");
	*bpout = NULL;
	*rpout = NULL;
}


/* find_boardname(boardname,bpout) ------------------------------------
	This routine finds the board whose name matches the one given.
*/
find_boardname(boardname,bpout)
char *boardname;
BOARDPTR *bpout;
{
	int i,j;
	CAGEPTR cp;
	BOARDPTR bp;
	char t[30],*s;

	s = t;
	while (*s++ = toupper(*boardname++));
	for (i = 0; i < ncages; i++) {
	    cp = cage_list[i];
	    for (j = 0; j < cp->nboards; j++) {
		bp = cp->board_list[j];
		if (strcmp(t,bp->name) == 0) {
		    *bpout = bp;
		    return;
		}
	    }
	}
	printf ("cannot find board name %s in %s\n",t,hw_tbl_name);
	*bpout = NULL;
}
