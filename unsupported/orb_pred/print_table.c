/****************************************************************
FUNCTION NAME:  print_table

SYNTAX: print_table(start,rev,day,hour,min,year,end)

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------
    rev		int[]		orbit values
    day		double[]	julian day value
    hour	double[]	julian hour value
    min		int[]		julian minute
    year	int[]	        year for each day

DESCRIPTION:
    
    Print out the table of orbit numbers and corresponding day and times.

RETURN VALUE:
    
    None.

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
     1.0  M. Shindle		original development

****************************************************************/

#include "asf.h"

#define MIN(a,b)  ((a) < (b) ? (a) : (b))
#define MAX(a,b)  ((a) > (b) ? (a) : (b))

extern int last_year;

void print_table(rev,day,hour,min,year,py)
int rev[];
double day[];
double hour[];
int min[];
int year[];
int py;
{
  int i;
  int start = 0, end = 4;    /* constants describing array bounds */

  /* check to see if row has different year */
  if (py == -1)
    for (i=start;i<end;i++) 
      if (year[i] != year[i+1]) {
        print_table(rev,day,hour,min,year,MIN(year[i],year[i+1]));
	print_table(rev,day,hour,min,year,MAX(year[i],year[i+1]));
	return;
      } else
	py = year[start];

  /* see if first column year equals last row's year */
  if (py != last_year) {
     printf("Year ==> %4d\n", py);
     last_year = py;
  }
  
  /* print elements */
  for (i=start;i<=end;i++) {
    if (year[i] != py) 
      printf("%16s"," ");
    else
      printf("%5d %3d %2d:%2d ", rev[i], (int)day[i], (int)hour[i], min[i]);
  }
  printf("\n");

  return;
}

