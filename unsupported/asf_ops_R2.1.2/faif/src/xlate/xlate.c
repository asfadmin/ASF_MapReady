/*   FILE:     xlate.c                   */
/*                                       */
/* Revision History:                     */
/* 6-Feb-1996 - VAH -  Initial Release   */
/* 7-Feb-1996 - VAH -  Bug fix for downlink -> datatake msg */

#include <stdio.h>
#include <string.h>
#include "xlate.h"

#define LINELEN 100

struct tblentry {
   char old_val[LINELEN];
   char new_val[LINELEN];
   char flag;
};

static int read_table(char *filename, struct tblentry *tbl);
static void prt_tbl(int cnt, struct tblentry *tbl);

int xlate(char *ifname, char *ofname, char *tblname)
{

FILE *ifp, *ofp, *tfp;
struct tblentry memtbl[1000];
char linein[LINELEN], lineout[LINELEN], *found;
int tblsiz, i, fm;

   if (strcmp(ifname, ofname) == 0)
      return(E_SAMEFILE);
   if ((ifp = fopen(ifname,"r")) == NULL)
      return(E_INFILE);
   if ((ofp = fopen(ofname,"w+")) == NULL)
      return(E_OUTFILE);

   tblsiz = read_table(tblname, memtbl);
   /* prt_tbl(tblsiz,memtbl); */

   if (tblsiz == -1) return(E_TBLFILE);
   if (tblsiz == 0) return(E_TBLFMT);

   while (fgets(linein, LINELEN, ifp))
   {
      if (linein[strlen(linein) -1] == '\n') linein[strlen(linein) -1] = '\0';

      strcpy(lineout,linein);
      for (found = NULL, i=0; i<tblsiz; i++)
      {
         found = strstr(linein,memtbl[i].old_val);
         if (found != NULL)	/* verify "full" match */
         {

            do {
               fm = ((linein == found) ||	/* left side */
                  (strchr(" \t\'\"",found[-1]) != NULL));
               fm &= 				/* rt side */
                  (strchr(" \t\'\"", found[strlen(memtbl[i].old_val)]) != NULL);
               if (!fm) found = strstr(&found[1],memtbl[i].old_val);
            } while ((found != NULL) && (!fm));

            if (fm && (memtbl[i].flag == 'D')) break;

            if (fm)
            { 
               if (found != linein)
               {
                  strncpy(lineout, linein, found-linein);  /* up to old val */
                  lineout[(found-linein)] = '\0';
               } else
                  strcpy(lineout,"");

               strcat(lineout, memtbl[i].new_val);     /* add new val */

               found += strlen(memtbl[i].old_val);
               if (*found != '\0') strcat(lineout, found);
               strcpy(linein,lineout);
            }
         }
      }
      if (memtbl[i].flag == 'D') continue;
      strcat (lineout,"\n");
      fputs(lineout, ofp);
   }
   fclose (ifp);
   fclose (ofp);
   return(0);
}

static int read_table(char *filename, struct tblentry *tbl)
{
FILE *fp;
char linein[LINELEN];
char *ptr;
int count, sres, i;

   if ((fp = fopen(filename,"r")) == NULL)
      return(-1);

   count = 0;
   while (fgets(linein, LINELEN, fp))
   {
      ptr = strchr(linein,'#');
      if (ptr == linein) continue;    /* first char is # */
      if (ptr != NULL) *ptr = '\0';   /* # after some chars */

      ptr = linein;
      while ((strchr(" \t\n\r",*ptr) != NULL) &&
             (*ptr != '\0')) ptr++;  /* skip  blanks*/

      /* READ FLAG */
      if (*ptr == '\0') 
         continue;      /* bad line */
      else
         tbl[count].flag = *ptr;
      ptr++;   

      while ((strchr(" \t\n\r",*ptr) != NULL) &&
             (*ptr != '\0')) ptr++;  /* skip  blanks*/

      /* READ "OLD_VALUE" */
      if (*ptr == '\0') 
         continue;      /* bad line */
      else {
         i=0;
         if (*ptr != '\"')		/* not in quotes */
         {
            while ((strchr(" \t\n\r",*ptr) == NULL) &&
                   (*ptr != '\0')) 
            {
               tbl[count].old_val[i++] = *ptr++;
            }
         } else {			/* item IS in quotes */
            ptr++;			/* skip leading quote */
            while ((*ptr != '\"') &&
                   (*ptr != '\0')) 
            {
               tbl[count].old_val[i++] = *ptr++;
            }
            if (*ptr == '\"') ptr++;	/* skip trailing quote */
         }
         tbl[count].old_val[i] = '\0';
      }

      /* READ "NEW VALUE" */
      if (tbl[count].flag != 'D')
      {
         while ((strchr(" \t\n\r",*ptr) != NULL) &&
                (*ptr != '\0')) ptr++;  /* skip  blanks*/
         if (*ptr == '\0') 
            continue;      /* bad line */
         else {
            i=0;
            if (*ptr != '\"')
               while ((strchr(" \t\n\r",*ptr) == NULL) &&
                      (*ptr != '\0')) 
                  tbl[count].new_val[i++] = *ptr++;
            else {
               ptr++;			/* skip leading quote */
               while ((*ptr != '\"') &&
                      (*ptr != '\0')) 
                  tbl[count].new_val[i++] = *ptr++;
               if (*ptr == '\"') ptr++;	/* skip trailing quote */
            }
            tbl[count].new_val[i] = '\0';
         }
      }
      count++; 
   }
   return(count);
}

static void prt_tbl(int cnt, struct tblentry *tbl)
{
int i;
   for (i=0; i<cnt; i++)
   {
      printf ("%c  \"%s\"  ->  \"%s\"\n",  tbl[i].flag,
         tbl[i].old_val, tbl[i].new_val);
   }
}
