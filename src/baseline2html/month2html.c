#include "asf.h"
#include "asf_meta.h"
#include "geolocate.h"
#include "dateUtil.h"

#define VERSION 1.0

void usage(char *name)
{
  printf("\n"
         "USAGE:\n"
         "   %s  <mode> <list>\n",name);
  printf("\n"
         "REQUIRED ARGUMENTS:\n"
	 "   mode   Beam mode\n"
         "   list   List of months with baselines.");
  printf("\n\n"
         "DESCRIPTION:\n"
         "   %s creates a web page per month with baselines.\n", 
	 name);
  printf("\n"
         "Version %.2f, ASF SAR Tools\n"
         "\n",VERSION);
  exit(EXIT_FAILURE);
}

int main(int argc, char *argv[])
{
  char monFile[255], *listFile;
  char buffer[1000], month[25000][15], m[15], *mode;
  int orbit[25000], year[25000];
  int i, j, n=0, y;
  FILE *fpList, *fpMon, *fpOut;
  quietflag=1;
  
  /* Parse command line args */
  if ((argc-currArg) < 2) {
    printf("Insufficient arguments.\n"); 
    usage(argv[0]);
  }

  mode = argv[1];
  listFile = argv[2];
  fpList = FOPEN(listFile, "r");
  fpOut = FOPEN("mode.lst", "w");  
  while (fgets(buffer, 1000, fpList)) {
    sscanf(buffer, "%d\t%s\t%d", &(orbit[n]), month[n], &(year[n]));
    n++;
  }

    for (i=0; i<n; i++) {
      sprintf(monFile, "%s%d.html", month[i], year[i]);
      fpMon = FOPEN(monFile, "w");
      fprintf(fpOut, "%s\t%d\n", month[i], year[i]);
      fprintf(fpMon, "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01"
	      " Transitional//EN\">\n");
      fprintf(fpMon, "<html>\n<head>\n");
      fprintf(fpMon, "<title>RADARSAT %s %s %d baselines</title>\n", 
	      mode, month[i], year[i]);
      fprintf(fpMon, "<meta http-equiv=\"Content-Type\""
	      " content=\"text/html; charset=iso-8859-1\">\n");
      fprintf(fpMon, "</head>\n<body>\n");
      fprintf(fpMon, "<h2>%s %s %d baselines</h2>\n", mode, month[i], year[i]);

      strcpy(m, month[i]);
      y = year[i];
      for (j=0; j<n; j++) {
        if (y == year[j] && strcmp(m, month[j])==0) {
          if (orbit[j] >= 10000)
	    fprintf(fpMon, "<a href=\"%s_%d.html\">%d</a><br>\n", mode, orbit[j], orbit[j]);
          else
	    fprintf(fpMon, "<a href=\"%s_0%d.html\">%d</a><br>\n", mode, orbit[j], orbit[j]);
        }
      }

      fprintf(fpMon, "<br><br>\n<a href=\"index.html\">Back to %s baseline listing</a>"
	      "<br><br>\n", mode);
      fprintf(fpMon, "<hr>\n");
      fprintf(fpMon, "<font size=1>\nCopyright &copy; 2004, ");
      fprintf(fpMon, "<A HREF=\"mailto:uso@asf.alaska.edu\">"
	      "Alaska Satellite Facility</a><br>\n");
      fprintf(fpMon, "Created on %s<br>\n", date_stamp());
      fprintf(fpMon,"</font>\n</body>\n</html>\n\n");

    FCLOSE(fpMon);

  }
  FCLOSE(fpList);
  FCLOSE(fpOut);
  
  return(0);
}
