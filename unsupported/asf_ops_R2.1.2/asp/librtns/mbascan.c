/* Alaska SAR Processor (ASP) %W% %E% %U% */
/*  mbascan.c -- Command line parser for Bobtalk */

#include <stdio.h>
#include <ctype.h>

extern int  n[20], nn, cmd;
extern char name[80];

#define WRITE_CMD 43
#define FUZZY_CMP_CMD 10
#define FUZZY_CMPR_CMD 11
#define H_CMD    35
#define HELP_CMD 44
#define QUES_CMD 45

/* ascan (s) -----------------------------------------------------------
	This routine scans the string s for a bobtalk opcode and
	operands, placing what it finds in global variables.  A number
	representing the command code is placed in cmd.  The numeric
	value of each operand is placed in the array n[].
*/
ascan (s)
char   *s;
{
    struct {
	char    *st;
	int     n;
	int     d;
	int     m;
    }       t[10];
    int     i, j, k, w, nt, c, nomatch, radix, num, cnum, name_cmd;
    char   *p;
    char   s1[200];

    /* add new commands at the end of cmdtab. Their position in
	the table will determine their case number in mbabob.c 
	and mbahelp.c */
    static char *cmdtab[] = {
			"s",   "ss",  "d",   "dr",  "df",
			"dfr", "m",   "mr",  "c",   "cr",
			"cf",  "cfr", "f",   "fs",  "fc",
			"fcs", "p",   "pr",  "p1",  "p1r",
			"ig",  "il",  "id",  "if",  "im",
			"imr", "ad",  "tm",  "t",   "pup",
			"i",   "a",   "v",   "n",   "b",
			"h",   "br",  "q",   "cg",  "ms",
			"fr",  "frs", "hi",  "w",   "help",
			"?",   "lo",  "csr", "dfi", "dfir",
			"ibob","sh",  "cd",  "dip", "vi",
			"is",  "dreg","sreg","dc",  "crr",
			"wd",  "wdr", "svp",
    };
    static int cmdtabn = sizeof(cmdtab) / sizeof(char *);

    /* also add commands to namecmds (below) if one of their arguments
	is an ascii string.  The string will be placed in the global
	variable "name".  Enter the position of the ascii argument
	in the corresponding entry in the table name_loc. */
    static char *namecmds[] = {
			"b",   "lo",  "cd",  "vi",  "is",
			"dreg","sreg","dc",  "w",   "wd",
			"wdr",
    };
    static int name_loc[]   = {
			 1,     1,     1,     1,     1,
			 1,     1,     1,     3,     3,
			 3,
    };
    static int nmclen = sizeof(namecmds) / sizeof(char *);


/* pass over s, locating words and storing start index, count in t */
    strcpy(s1,s);
    for (i = 0; i < 10; i++) {	/* init word array */
	t[i].d = 0;
	t[i].n = 0;
	t[i].m = 0;
    }
    w = 1;			/* turn whitespace on to start */
    j = (-1);			/* token index is incremented before use */
    for (p = s; *p && *p != ';' && (j < 10); p++) {
	if (isupper(c = *p))
	    *p = c = tolower(c);
	if (isalnum(c) || (strchr("-/_.*#?",c) != NULL)) {
	    if (w) {
		j++;
		t[j].st = p;
		t[j].n = 0;
		w = 0;
	    }
	    t[j].n++;
	    t[j].m |= (c == '-');
	    t[j].d |= ((c == '*') || (c == '#'));
	}
	else {
	    *p = '\0';
	    *(p + (s1 - s)) = '\0';
	    w = 1;
	}
    }
    if ((nt = j) < 0) {		/* check for nothing on the line */
	cmd = 254;
	return;
    }

/* locate command in cmdtab */
    cmd = 255;			/* init cmd to "unrecognizeable" */
    for (i = 0; i < cmdtabn; i++) {
	if (strcmp(t[0].st,cmdtab[i]) == 0) {
	    cmd = i;
	    break;
	}
    }

/* perform special treatment for certain command arguments */
    /* see if this is a "name" command */
    name_cmd = 0;
    for (i = 0; i < nmclen; i++) {
	if (strcmp(t[0].st,namecmds[i]) == 0) {
	    name_cmd = name_loc[i];
	    break;
	}
    }

    /* if name command, copy ascii argument to "name" global variable */
    if (name_cmd) {
	if (nt >= name_cmd)
	    strcpy (name, t[name_cmd].st + (s1 - s));
	else
	    name[0] = '\0';
	if (strlen(name) < 1)
	    cmd = cmdtabn ;
    }
    if (cmd == WRITE_CMD)
	t[1].d = t[2].d = 1;	/* force line #s to be decimal */
    if (cmd == FUZZY_CMP_CMD || cmd == FUZZY_CMPR_CMD)
	t[4].d = t[5].d = 1;    /* force args 4 & 5 to be decimal */
    if (cmd == H_CMD || cmd == HELP_CMD || cmd == QUES_CMD) {
	if (nt < 1)
	    n[0] = -1;
	else {
	    n[0] = atoi(t[1].st);
	    if (*t[1].st != '0' && n[0] == 0) {
		n[0] = 255;	     /* init cmd to "unrecognizeable" */
		for (i = 0; i < cmdtabn; i++)
		{
		    if (strcmp(t[1].st,cmdtab[i]) == 0)
		    {
			n[0] = i;
			break;
		    }
		}
	    }
	}
	nn = nt;
	return;
    }
	
/* convert numeric parameters */
    for (i = 0; i < 10; i++) n[i] = 0;
    for (i = 1; i <= nt; i++) {
	radix = (t[i].d) ? 10 : 16;
	num = 1;
	for (p = t[i].st; *p; p++) {
	    c = *p;
	    cnum = 0;
	    if (isdigit(c)) {
		c = c - '0';
		cnum = 1;
	    } else {
		if (c >= 'a' && c <= 'f' && radix == 16) {
		    c = c - 'a' + 10;
		    cnum = 1;
		} else if (c != '-' && c != '*' && c != '#') num = 0;
	    }
	    if (cnum) n[i-1] = n[i-1] * radix + c;
	}
	if (num == 0) {
	    n[i-1] = 0;
	    if (name_cmd && i == name_cmd)  /* if this arg is ascii */
		num = 1;
	}
	if (num == 0) printf("operand %d not numeric\n",i);
	if (t[i].m) n[i-1] = - n[i-1];
    }
    nn = nt;
    return;
}
