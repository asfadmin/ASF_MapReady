/* file xlate.h */
/* included by programs wishing to use the wos/sv translator */

/* possible errors */
#define E_SAMEFILE -101		/* input & ouput must be different */
#define E_INFILE -102		/* can't open input file */
#define E_OUTFILE -103		/* can't open output file */
#define E_TBLFILE -104		/* can't open table file */
#define E_TBLFMT -105		/* got no valid table entries */

int xlate(char *ifname, char *ofname, char *tblnam);

/*  the table is an ascii file with replace and delete lines ...
/* delete lines have the following format:
/*
/*  D <OLD_STRING> 
/*
/* lines from the input file containing the string <OLD_STRING> will
/* be deleted
/*
/* replace lines have the following format
/*  R <OLD_STRING> <NEW_STRING>
/*
/* occurrences of <OLD_STRING> will be replaced by <NEW_STRING>
/*
/* multiple "replace" lines can act on the same input line
/* (i.e. you can replace the keyword & value )
/*
/* no validation is performed on the file
/**/
