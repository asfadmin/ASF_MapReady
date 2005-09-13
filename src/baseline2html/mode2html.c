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
         "   %s creates a web page with a list of months that have baselines.\n", 
	 name);
  printf("\n"
         "Version %.2f, ASF SAR Tools\n"
         "\n",VERSION);
  exit(EXIT_FAILURE);
}

int main(int argc, char *argv[])
{
  char *listFile;
  char buffer[1000], month[25], *mode;
  int year;
  FILE *fpList, *fpMon;
  quietflag=1;
  
  /* Parse command line args */
  if ((argc-currArg) < 2) {
    printf("Insufficient arguments.\n"); 
    usage(argv[0]);
  }

  mode = argv[1];
  listFile = argv[2];
  fpList = FOPEN(listFile, "r");
  fpMon = FOPEN("index.html", "w");  
  fprintf(fpMon, "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01"
	  " Transitional//EN\">\n");
  fprintf(fpMon, "<html>\n<head>\n");
  fprintf(fpMon, "<title>RADARSAT %s baselines</title>\n", mode);
  fprintf(fpMon, "<meta http-equiv=\"Content-Type\""
	  " content=\"text/html; charset=iso-8859-1\">\n");

  /* StatCounter script for monitoring traffic */
  fprintf(fpMon, "<!-- Start of StatCounter Code -->\n");
  fprintf(fpMon, "<script type=\"text/javascript\" language=\"javascript\">\n");
  fprintf(fpMon, "var sc_project=573031;\n"); 
  fprintf(fpMon, "var sc_partition=4;\n"); 
  fprintf(fpMon, "var sc_security=\"ec51af54\";\n"); 
  fprintf(fpMon, "var sc_invisible=1;\n");
  fprintf(fpMon, "</script>\n\n");
  fprintf(fpMon, "<script type=\"text/javascript\" language=\"javascript\" "
	  "src=\"http://www.statcounter.com/counter/counter.js\"></script>"
	  "<noscript><a href=\"http://www.statcounter.com/\" target=\"_blank\">"
	  "<img  src=\"http://c5.statcounter.com/counter.php?"
	  "sc_project=573031&amp;amp;java=0&amp;amp;security=ec51af54&amp;amp;invisible=1\" "
	  "alt=\"counter\" border=\"0\"></a> </noscript>\n");
  fprintf(fpMon, "<!-- End of StatCounter Code -->\n");
  /* End of StatCounter business */

  fprintf(fpMon, "</head>\n<body>\n");
  fprintf(fpMon, "<h2>%s baselines</h2>\n", mode);

  fprintf(fpMon, "<a href=\"%s_baseline.tar.gz\">All %s baselines in plain text</a><br>\n", 
	  mode, mode);
  fprintf(fpMon, "<a href=\"%s_shape.tar.gz\">All %s baselines in shape files</a><br><br>\n",
	  mode, mode);
  while (fgets(buffer, 1000, fpList)) {
    sscanf(buffer, "%s\t%d", month, &year);
    fprintf(fpMon, "<a href=\"%s%d.html\">%s %d</a><br>\n", month, year, month, year);
  }

  fprintf(fpMon, "<br><br>\n<a href=\"../text_only.html\">Back to Radarsat baseline catalog</a>"
	  "<br><br>\n");
  fprintf(fpMon, "<hr>\n");
  fprintf(fpMon, "<font size=1>\nCopyright &copy; 2004, ");
  fprintf(fpMon, "<A HREF=\"mailto:uso@asf.alaska.edu\">"
	  "Alaska Satellite Facility</a><br>\n");
  fprintf(fpMon, "Created on %s<br>\n", date_stamp());
  fprintf(fpMon,"</font>\n</body>\n</html>\n\n");

  FCLOSE(fpMon);
  FCLOSE(fpList);
  
  return(0);
}
