#include <stdio.h>
#include "xlate.h"
#define FNLEN 800	/* length of filenames */

main(int argc, char **argv)
{
char ifnam[FNLEN], ofnam[FNLEN], tfnam[FNLEN];
int stat;

    ifnam[0] = '\0';
    ofnam[0] = '\0';
    tfnam[0] = '\0';

   if (argc > 1) 
      strcpy(ifnam,argv[1]);

   if (argc > 2) 
      strcpy(ofnam,argv[2]);

   if (argc > 3) 
      strcpy(tfnam,argv[3]);

   stat = xlate(ifnam, ofnam, tfnam);
   if (stat < 0)
   {
      switch (stat)
      {
      case E_SAMEFILE:
         printf ("Infile and Outfile must be different\n");
         break;
      case E_INFILE:
         printf ("Can't open input file %s\n",ifnam);
         break;
      case E_OUTFILE:
         printf ("Can't open output file %s\n",ofnam);
         break;
      case E_TBLFILE:
         printf ("Can't open table file %s\n",tfnam);
         break;
      case E_TBLFMT:
         printf ("No table entries found in file %s\n",tfnam);
         break;
      }
   exit(-1);
   }
   exit(0);
}
