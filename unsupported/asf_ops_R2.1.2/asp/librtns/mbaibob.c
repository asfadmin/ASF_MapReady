/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* mbaibob.c - interactive graphic debugger

	This routine is the interactive bob-oriented hardware
	debugger.  It provides a graphical interface to the
	hardware system under testing, with displays of cage, board,
	register, bitfield and function memory names and contents.
	Several convenience features are available, including testing
	the presence or absence of boards, quick setup of the test
	pipe, and loading and saving register and/or function memory
	contents from/to disk files.

	IBOB senses whether the user is on a graphics screen or not.  
	If not, IBOB says so and returns to the caller.
*/

#include <ibobdefs.h>
#include <aspdecl.h>
#include <stdio.h>
/*#include <errors.h> */
#include <signal.h>

char system_name[50];		/* name of system being tested */
int tnibl = 8;			/* multibus address nibble (8 or 9) */

int ncages = 0;			/* number of cages defined */
CAGEPTR cage_list[20];		/* cage data table */

WATCHPTR watch_list = NULL;	/* watch address list */
int watch_sw = 0;		/* 1 = selecting watch object */

int max_cname = 0;		/* max length of cage name */
int max_bname = 0;		/* max length of board name */

typedef enum {regs, fields} smode;
smode screen_mode = regs;	/* screen display mode */

extern int cur_page;		/* currently displayed page */
extern BOARDPTR selected_bp;	/* currently selected board */

extern toupper_sw;		/* 1 = convert lower case to upper */

char regs_file[50];		/* register load file name */
int regs_modified = 0;		/* 1 = modified since loaded */
typedef enum {previous, init, loadfile} rsource;
rsource regs_source = previous;	/* register contents source */

extern int regx;		/* loc. of left edge of sub-window */
extern int minpix,minlin,maxpix,maxlin;  /* screen boundary */
extern int bigfont;		/* font id's */

#define HWNAMELEN   80
char hw_tbl_name[HWNAMELEN] = "/usr/local/asp/hardware.dat";

/* ibob () -------------------------------------------------------------
	This routine is the bobtalk interactive debugger main line.
*/

ibob()
{
	int l,c;
	int trap_errors();

	get_hardware_table();
    /*
	print_hardware_table();
    */
	if (init_ibob_graphics() == 0) {
	    printf("\nNot a graphics terminal\n\n");
	    return;
	}
	else {
	    open_keyboard();
	    trap_errors(-1);
	/*
	    seterrtrap(trap_errors,1);
	*/
	    main_loop();
	    trap_errors(-2);
	    close_keyboard();
	    end_ibob_graphics();
	}
}

/* main_loop() -------------------------------------------------------
	This routine contains the main processing loop:
		Get input from keyboard/mouse
		Interpret input
		Perform requested command, if any
		Return to top of loop
*/
main_loop()
{
	char c[1];		/* input character from keyboard */
	int button = 0;		/* button press data */
	int id,val;		/* returned values from pick */

	while (1) {
	    c[0] = '\0';
	    button = get_input(c); /* read input from keyboard/mouse */
	    id = c[0];
	    pick (button,&id,&val);
	    if (id == 0 && val == 6)	/* quit command entered? */
		return (1);
	    do_command (button,id,val);
	    watch();
	}
	return (1);
}

/* do_command(button,id,val) -----------------------------------------
	This routine performs the command requested by the user's
	keyboard/mouse entry, as specified in the input parameters.
*/
do_command(button,id,val)
	int button,id,val;
{
	BOARDPTR bp,find_board();
	REGPTR   rp,find_reg();
	FIELDPTR fp,find_field();
	int i;

	if (button == 0 && id != 0)
	    return;
	switch (id) {
	    case 0:		/* menu picks */
		switch (val) {
		    case 1:		/* Presence */
			test_presence(NULL);
			draw_presence();
			break;
		    case 2:		/* Initialize */
		/*	set_initial_values();  */
			pup();
			draw_rack(cur_page);
			break;
		    case 3:		/* Save regs */
			save_registers();
			break;
		    case 4:		/* Load regs */
			load_registers();
			break;
		    case 5:		/* Watch (something) */
			watch_sw = 1;
			break;
		    default:
			break;
		}
		break;
	    case 99:		/* no pick */
		break;
	    default:		/* screen lines */
		bp = find_board(id);
		if (screen_mode == regs) {
		    switch (val) {
			case 3:		/* board name */
			    if (bp != NULL) {
				screen_mode = fields;
				draw_fields(bp);
			    }
			    break;
			case 4:		/* test bus */
			    setup_test_bus(bp,1);
			    break;
			case 1:		/* cage name */
			    draw_rack(cur_page);
			    break;
			case 2:		/* presence lights */
			    if (bp != NULL)
				test_presence(bp);
			    draw_presence();
			    break;
			default:	/* registers */
			    if (bp != NULL)
				change_reg(bp,val,button);
			    break;
		    }  /* switch val */
		}  /* if screen_mode */
		else
		if (screen_mode == fields) {
		    switch (val) {
			case 3:		/* board name */
			    if (bp != NULL)
				draw_fields(bp);
			    break;
			case 1:		/* cage name */
			    screen_mode = regs;
			    selected_bp = NULL;
			    draw_rack(cur_page);
			    break;
			case 2:		/* presence lights */
			    if (bp != NULL)
				test_presence(bp);
			    draw_presence();
			    break;
			case 4:		/* test bus */
			    setup_test_bus(bp,1);
			    break;
			default:	/* fields */
			    change_field(selected_bp,val,id,button);
			    change_memory(selected_bp,val,id,button);
			    break;
		    }  /* switch val */
		}  /* else if screen_mode */
	}  /* switch id */
}

/* get_hardware_table() ------------------------------------------------
	This routine reads the file "hardware.dat", which contains
	the description of the system being tested.  Specifically, the
	file contains the system name, and descriptions of each cage
	and board in the system.  This data is loaded into the hardware
	table for use by the program.
*/
get_hardware_table()
{
	CAGEPTR cp = NULL;
	BOARDPTR bp = NULL;
	REGPTR rp = NULL;
	REGPTR rp2;
	FIELDPTR fp = NULL;
	FIELDPTR fp2;
	MEMPTR mp = NULL;
	MEMPTR mp2;
	char t[80];
	int i,j,k,n,type;
	void *malloc(),*realloc();
	static int first = 1;

	if (first == 0) return;
	if (open_file(hw_tbl_name,"") == 0) {
	    printf("cannot open %s file\n",hw_tbl_name);
	    exit (1);
	}
	set_separators(",");
	toupper_sw = 1;
	t[0] = '\0';
	while (next_head(t)) {
	    type = head_type(t);
	    switch (type) {
		case 1:		/* system */
		    next_token(system_name);
		    get_int(&tnibl);
		    break;
		case 2:		/* cage */
		    bp = NULL;
		    rp = NULL;
		    fp = NULL;
		    mp = NULL;
		    if ((cp = (CAGEPTR) malloc(sizeof(CAGE))) == NULL) {
			printf("out of memory in cage alloc\n");
			exit (2);
		    }
		    next_token(cp->name);
		    if ((n = strlen(cp->name)) > max_cname) max_cname = n;
		    cp->nboards = 0;
		    cage_list[ncages++] = cp;
		    break;
		case 3:		/* board */
		    rp = NULL;
		    fp = NULL;
		    mp = NULL;
		    if (cp == NULL) {
			line_error("BOARD","CAGE");
			break;
		    }
		    if ((bp = (BOARDPTR) malloc(sizeof(BOARD))) == NULL) {
			printf("out of memory in board alloc\n");
			exit (2);
		    }
		    cp->board_list[cp->nboards++] = bp;
		    next_token(bp->name);
		    if ((n = strlen(bp->name)) > max_bname) max_bname = n;
		    get_int(&bp->nregs);
		    bp->mbus = 0;
		    bp->byteregs = 0;
		    n = get_int(&bp->addr);
		    while (n < 3) {
			n = next_token(t);
			if (t[0] == 'M') bp->mbus = 1;
			if (t[0] == 'B') bp->byteregs = 1;
		    }
		    bp->status = 0;
		    bp->reg_list = NULL;
		    bp->mem_list = NULL;
		    bp->taps = 0;
		    break;
		case 4:		/* register */
		    fp = NULL;
		    if (bp == NULL) {
			line_error("REGISTER","BOARD");
			break;
		    }
		    if ((rp2 = (REGPTR) malloc(sizeof(REGISTER))) == NULL) {
			printf("out of memory in register alloc\n");
			exit (2);
		    }
		    if (rp == NULL)
			bp->reg_list = rp2;
		    else
			rp->nxt_reg = rp2;
		    rp = rp2;
		    get_int(&rp->num);
		    rp->addr = bp->addr + ((2 - bp->byteregs) * rp->num);
		    rp->nxt_reg = NULL;
		    rp->taps = 0;
		    rp->nfields = 0;
		    rp->initval = 0;
		    rp->watchptr = NULL;
		    rp->readonly = 1;
		    break;
		case 5:		/* field */
		    if (rp == NULL) {
			line_error("FIELD","REGISTER");
			break;
		    }
		    if ((fp2 = (FIELDPTR) malloc(sizeof(FIELD))) == NULL) {
			printf("out of memory in field alloc\n");
			exit (2);
		    }
		    if (fp == NULL)
			rp->field_list = fp2;
		    else
			fp->nxt_field = fp2;
		    fp = fp2;
		    fp->nxt_field = NULL;
		    rp->nfields++;
		    next_token(fp->name);
		    get_int(&fp->startbit);
		    get_int(&fp->nbits);
		    t[0] = '\0';
		    if (get_int(&fp->initval) < 3) next_token(t);
		    fp->tap = (t[0] == 'T');
		    fp->readonly = (t[0] == 'R');
		    fp->bypass = (t[0] == 'B');
		    rp->taps += fp->tap;
		    bp->taps += fp->tap;
		    i = strlen(fp->name);
		    fp->inverted = (fp->name[i-1] == '*');
		    fp->shift = fp->startbit - fp->nbits + 1;
		    i = 0xffff >> (16 - fp->nbits);
		    fp->mask = i << fp->shift;
		    fp->initval &= i;
		    rp->initval |= fp->initval << fp->shift;
		    rp->readonly &= fp->readonly;
		    break;
		case 6:		/* memory */
		    fp = NULL;
		    if (bp == NULL) {
			line_error("MEMORY","BOARD");
			break;
		    }
		    if ((mp2 = (MEMPTR) malloc(sizeof(MEMORY))) == NULL) {
			printf("out of memory in memory alloc\n");
			exit (2);
		    }
		    if (mp == NULL)
			bp->mem_list = mp2;
		    else
			mp->nxt_mem = mp2;
		    mp = mp2;
		    mp->nxt_mem = NULL;
		    next_token(mp->name);
		    get_int(&mp->page);
		    get_int(&mp->addr);
		    mp->disp_addr = mp->addr;
		    get_int(&mp->len);
		    get_int(&mp->disp_len);
		    n = get_hex(&mp->mask);
		    mp->readonly = 0;
		    mp->writeonly = 0;
		    mp->init_file[0] = '\0';
		    mp->load_file[0] = '\0';
		    mp->modified = 0;
		    mp->watchptr = NULL;
		    while (n < 3) {
			n = next_token(t);
			if (t[0] == 'R')	/* readonly */
			    mp->readonly = 1;
			if (t[0] == 'W')	/* writeonly */
			    mp->writeonly = 1;
			if (t[0] == 'I') {	/* initfile=filename */
			    k = 0;
			    while (t[k] |= '\0')  /* find = */
				if (t[++k] == '=') break;
			    while (t[k] |= '\0')  /* find name start */
				if (t[++k] |= ' ') break;
			    strcpy(mp->init_file,&t[k]);
			}
		    }  /* while n */
		    break;
		case 9:		/* END. */
		    break;
		default:
		    printf ("unidentifiable line\n");
		    break;
	    }  /* switch */
	}  /* while */
	close_file();
	first = 0;
}

/* print_hardware_table() --------------------------------------------
	This routine prints a debugging dump of the hardware table.
*/
print_hardware_table()
{
	CAGEPTR cp;
	BOARDPTR bp;
	REGPTR rp;
	FIELDPTR fp;
	MEMPTR mp;
	int i,j;

	if (tnibl) printf("%s system at multibus address %d\n",system_name,
			tnibl);
	else printf("SYSTEM description line missing *****\n");
	for (i = 0; i < ncages; i++) {
	    cp = cage_list[i];
	    printf("%s cage, ",cp->name);
	    pluralize("board",cp->nboards);
	    printf(":\n");
	    for (j = 0; j < cp->nboards; j++) {
		bp = cp->board_list[j];
		printf("  %s board, ",bp->name);
		pluralize("register",bp->nregs);
		printf(" at 0x%.3X, mbus = %s",bp->addr,
			bp->mbus ? "yes, " : "no, ");
		pluralize("tap",bp->taps);
		printf("\n");
		for (rp = bp->reg_list; rp != NULL; rp = rp->nxt_reg) {
		    printf("    Register %d, ",rp->num);
		    pluralize("field",rp->nfields);
		    printf(" at 0x%.3X, ",rp->addr);
		    pluralize("tap",rp->taps);
		    printf("\n");
		    for (fp = rp->field_list; fp != NULL;
				fp = fp->nxt_field) {
			printf("      %s field, at bit %d for ",
			    fp->name,fp->startbit);
			pluralize("bit",fp->nbits);
			printf(", init=%d",fp->initval);
			printf("%s",fp->inverted ? ", inverted" : "");
			printf("%s",fp->readonly ? ", read only" : "");
			printf("%s",fp->bypass ? ", bypass" : "");
			printf("%s\n",fp->tap ? ", test tap" : "");
		    }  /* for fp */
		}  /* for rp */
		for (mp = bp->mem_list; mp != NULL; mp = mp->nxt_mem) {
		    printf("    Memory %s, at page %d, addr 0x%.5x",
			    mp->name,mp->page,mp->addr);
		    printf(", len=%d, disp_len=%d, mask=%.4x",mp->len,
			    mp->disp_len, mp->mask);
		    printf("%s",mp->readonly ? ", read only" : "");
		    printf("%s",mp->writeonly ? ", write only" : "");
		    if (mp->init_file[0] |= '\0')
			printf(", init file = %s",mp->init_file);
		    printf("\n");
		}  /* for mp */
	    }  /* for j */
	}  /* for i */
}

/* set_hw_tbl_name(name) -----------------------------------------------
	This routine changes the name of the hardware table file to
	the given name.
*/
set_hw_tbl_name(name)
	char *name;
{
	if (strlen(name) < HWNAMELEN)
	    strcpy(hw_tbl_name,name);
	else {
	    printf("New hardware table name over %d characters long -- \n",
		HWNAMELEN);
	    printf("  using %s instead\n",hw_tbl_name);
	}
}

/* pluralize(t,d) ----------------------------------------------------
	This routine prints the decimal number d, followed by the
	word(s) in t.  if d != 1, an 's' is appended to t.
*/
pluralize(t,d)
	char *t;
	int d;
{
	printf("%d %s",d,t);
	if (d != 1) printf("s");
}

/* head_type(t) ------------------------------------------------------
	This routine identifies the type of the line heading word found
	in the string t.
*/
head_type(t)
	char *t;
{
	if (strcmp(t,"SYSTEM") == 0) return (1);
	if (strcmp(t,"CAGE") == 0) return (2);
	if (strcmp(t,"BOARD") == 0) return (3);
	if (strcmp(t,"REGISTER") == 0) return (4);
	if (strcmp(t,"FIELD") == 0) return (5);
	if (strcmp(t,"MEMORY") == 0) return (6);
	if (strcmp(t,"END.") == 0) return (9);
	return (0);
}

/* line_error(t1,t2) -------------------------------------------------
	This routine prints an input line found to be in error.
*/
line_error(t1,t2)
	char *t1,*t2;
{
	char t[100];
	int n;

	printf("%s line with no preceding %s line:\n",t1,t2);
	printf("    %s",t1);
	n = 0;
	while (n < 3) {
	    next_token(t);
	    printf(",%s");
	}
	printf("\n");
}

/* test_presence (bp) ---------------------------------------------
	This routine tests whether the given board is present or not.
	If the board pointer is NULL, all the boards are tested.
	If all boards tested are present, the routine returns PASS;
	otherwise FAIL is returned.
*/
test_presence (bp)
	BOARDPTR bp;
{
	CAGEPTR cp;
	int i,j,ans;

	get_hardware_table();
	if (bp != NULL) return(test_reg(bp));

	ans = PASS;
	for (i = 0; i < ncages; i++) {
	    cp = cage_list[i];
	    for (j = 0; j < cp->nboards; j++)
		if (test_reg(cp->board_list[j]) == FAIL) ans = FAIL;
	}
	return (ans);
}

/* test_reg(bp) --------------------------------------------------------
	This routine tests the presence of the given board by trying
	to set its first writable register.  If the register can be 
	set, the board is considered to be present and the board's 
	status flag is set to 1, otherwise the status flag is set to 0.
	The routine returns PASS if the board is present, FAIL if not.
*/
test_reg(bp)
BOARDPTR bp;
{
	REGPTR rp;
	int i;
	short int j;

	for (rp = bp->reg_list; rp != NULL; rp = rp->nxt_reg) {
	    if (rp->readonly == 0) {
		i = rp->addr >> 1;
		break;
	    }
	}
	if (rp == NULL) return (PASS);
	asp_read( i<<1, &mb.w[i], 2 );
	j = mb.w[i];
	mb.w[i] = ~j;
	asp_write( i<<1, &mb.w[i], 2 );
	asp_read( i<<1, &mb.w[i], 2 );
	bp->status = (mb.w[i] != j);
	mb.w[i] = j;
	asp_write( i<<1, &mb.w[i], 2 );
	return (bp->status & 1 ? PASS : FAIL);
}

/* set_initial_values() -----------------------------------------------
	This routine sets all the registers to their initial values
	as specified in the hardware.dat file
*/
set_initial_values()
{
	CAGEPTR cp;
	BOARDPTR bp;
	REGPTR rp;
	MEMPTR mp;
	int i,j;

	for (i = 0; i < ncages; i++) {
	    cp = cage_list[i];
	    for (j = 0; j < cp->nboards; j++) {
		bp = cp->board_list[j];
		for (rp = bp->reg_list; rp != NULL; rp = rp->nxt_reg)
		    set_reg(bp,rp,rp->initval);
		for (mp = bp->mem_list; mp != NULL; mp = mp->nxt_mem) {
		    if (strlen(mp->init_file))
			load_memory(mp,mp->init_file);
		}  /* for mp */
	    }  /* for j */
	}  /* for i */
	regs_source = init;
	regs_modified = 0;
}

/* setup_test_bus(bpin,iopt) -------------------------------------------
	This routine sets up the test bus so that the given board is the
	last board in the pipe.  All test taps before the given board
	are reset, the next test tap in line is set, and any boards
	between this board and the next test tap are bypassed.
	If iopt = 1, the screen is redrawn.
*/
setup_test_bus(bpin,iopt)
	BOARDPTR bpin;
	int iopt;
{
	CAGEPTR cp;
	BOARDPTR bp;
	REGPTR rp;
	FIELDPTR fp;
	int i,j,rval,val;
	int before_board = 1;
	int after_board = 0;

	for (i = 0; i < ncages; i++) {
	    cp = cage_list[i];
	    for (j = 0; j < cp->nboards; j++) {
		bp = cp->board_list[j];
		if (bp == bpin) before_board = 0;
		for (rp = bp->reg_list; rp != NULL; rp = rp->nxt_reg) {
		    for (fp = rp->field_list; fp != NULL;
				    fp = fp->nxt_field) {
			if (fp->tap)
			    set_field(bp,rp,fp,!(before_board));
			if (fp->bypass && after_board)
			    set_field(bp,rp,fp,1);
		    }  /* for fp */
		}  /* for rp */
		if (bp == bpin)
		    after_board = 1;
	    }  /* for j */
	}  /* for i */
	if (iopt) draw_screen();
}

/* set_field(bp,rp,fp,val) ---------------------------------------------
	This routine sets the given field to the value in val.
*/
set_field(bp,rp,fp,val)
	BOARDPTR bp;
	REGPTR rp;
	FIELDPTR fp;
	int val;
{
	int rval;

	if (fp->inverted) val = ~val;
	rval = get_reg(bp,rp);
	rval &= ~(fp->mask);
	rval |= (val << fp->shift) & fp->mask;
	set_reg(bp,rp,rval);
}

/* set_reg(bp,rp,rval) -------------------------------------------------
	This routine sets the value of the given register to rval.
*/
set_reg(bp,rp,rval)
BOARDPTR bp;
REGPTR rp;
int rval;
{
	asp_read( rp->addr & 0xfffffffe, &mb.w[ rp->addr >>1 ], 2 );
	if (bp->byteregs) mb.b[rp->addr] = rval & 0xff;
	else mb.w[rp->addr >> 1] = rval & 0xffff;
	asp_write( rp->addr & 0xfffffffe, &mb.w[ rp->addr >>1 ], 2 );
	regs_modified = 1;
}

/* get_field(bp,rp,fp) ------------------------------------------------
	This routine returns the value of the requested field.
*/
get_field(bp,rp,fp)
	BOARDPTR bp;
	REGPTR rp;
	FIELDPTR fp;
{
	int rval;

	rval = get_reg(bp,rp);
	if (fp->inverted) rval = ~rval;
	return ((rval & fp->mask) >> fp->shift);
}

/* get_reg(bp,rp) ---------------------------------------------------
	This routine returns the value of the requested register.
*/
get_reg(bp,rp)
	BOARDPTR bp;
	REGPTR rp;
{
	int rval;

	asp_read( rp->addr & 0xfffffffe, &mb.w[ rp->addr >>1 ], 2 );
	if (bp->byteregs) rval = mb.b[rp->addr] & 0xff;
	else rval = mb.w[rp->addr >> 1] & 0xffff;
	return (rval);
}

/* save_registers() ---------------------------------------------------
	This routine saves all the registers to a disk file.
*/
save_registers()
{
	CAGEPTR cp;
	BOARDPTR bp;
	REGPTR rp;
	FILE *fp;
	char filename[80],edit_pattern[100];
	int i,j,rval;
	char p = '%';

	get_filename("Enter save file name:",filename,".reg");
	if (strlen(filename) == 0) {
	    draw_menu();
	    return;
	}
	if ((fp = fopen(filename,"w")) == NULL) {
	    print_error("Cannot open file");
	    return;
	}
	sprintf(edit_pattern,"%c-%ds",p,max_bname);
	for (i = 0; i < ncages; i++) {
	    cp = cage_list[i];
	    for (j = 0; j < cp->nboards; j++) {
		bp = cp->board_list[j];
		fprintf(fp,edit_pattern,bp->name);
		for (rp = bp->reg_list; rp != NULL; rp = rp->nxt_reg) {
		    rval = get_reg(bp,rp);
		    if (bp->byteregs)
			fprintf(fp,",%.2X",rval);
		    else
			fprintf(fp,",%.4X",rval);
		}  /* for rp */
		fprintf(fp,"\n");
	    }  /* for j */
	}  /* for i */
	fclose(fp);
	draw_menu();
}

/* load_registers() ---------------------------------------------------
	This routine loads all the registers from a disk file.
*/
load_registers()
{
	CAGEPTR cp;
	BOARDPTR bp;
	REGPTR rp;
	char s[80],t[80],filename[80];
	int i,j,m,n,rval;

	get_filename("Enter load file name:",filename,".reg");
	if (strlen(filename) == 0) {
	    draw_menu();
	    return;
	}
	if (open_file(filename,"") == 0) {
	    print_error("Cannot open file");
	    return;
	}
	for (i = 0; i < ncages; i++) {
	    cp = cage_list[i];
	    for (j = 0; j < cp->nboards; j++) {
		bp = cp->board_list[j];
		n = next_token(t);
		if (strcmp(t,bp->name)) {
		    sprintf(s,"name mismatch on %s board",t);
		    print_error(s);
		    close_file();
		    return;
		}
		if (n == 3) n = 1;
		for (rp = bp->reg_list; rp != NULL; rp = rp->nxt_reg) {
		    m = next_token(t);
		    if ((m == 0) || (n == 3)) {
			sprintf(s,"Incorrect # of regs for %s board",
				bp->name);
			print_error(s);
			close_file();
			return;
		    }  /* if m */
		    n = m;
		    sscanf(t,"%X",&rval);
		    set_reg(bp,rp,rval);
		}  /* for rp */
	    }  /* for j */
	}  /* for i */
	close_file();
	regs_source = loadfile;
	strcpy(regs_file,filename);
	regs_modified = 0;
	draw_screen();
	draw_menu();
}

/* get_filename(prompt,filename,ext) ----------------------------------
	This routine prompts the user for a filename, reads it, and
	makes sure it has the extension given in ext.
*/
get_filename(prompt,filename,ext)
	char *prompt,*filename,*ext;
{
	int flen,elen;

	filename[0] = '\0';
	toupper_sw = 0;
	get_string(filename,prompt);
	toupper_sw = 1;
	if ((flen = strlen(filename)) == 0) return;
	if ((elen = strlen(ext)) > flen) {
	    strcat(filename,ext);
	    return;
	}
	if (strcmp(filename + flen - elen,ext)) strcat(filename,ext);
}

/* memcopy (s,t,n) -----------------------------------------------------
	This routine copies n bytes from t to s.
*/
memcopy (s,t,n)
	char *s,*t;
	int n;
{
	while (n--) *s++ = *t++;
}

/* trap_errors(opt) ----------------------------------------------------
	This routine handles errors and abort conditions.  There are
	three possible options:
	1) if opt = -1, this routine initializes the signal routine
	    to trap all relevant error signals and come here.
	2) if opt = -2, the default signal processing actions are
	    restored (core dumps, etc.).
	3) any other value means a signal has been received.  In this
	    case, the routine cleans up the graphics screen and exits.
*/
trap_errors(opt)
	int opt;
{
	static int sig[] = { SIGHUP, SIGINT, SIGQUIT, SIGILL,
		             SIGIOT, SIGEMT, SIGFPE,  SIGBUS,
		             SIGSEGV,SIGSYS
		           };
	int nsigs = sizeof(sig) / sizeof(int);
	int i;

	if (opt == -1) {	/* set to trap errors here */
	    for (i = 0; i < nsigs; i++) signal(sig[i],trap_errors);
	    return;
	}
	if (opt == -2) {	/* reset to default action */
	    for (i = 0; i < nsigs; i++) signal(sig[i],SIG_DFL);
	    return;
	}

    /* this code is the actual signal trap routine */
	close_keyboard();
	end_ibob_graphics();
	printf("Program terminated, signal = %d\n",opt);
	exit(1);
}
