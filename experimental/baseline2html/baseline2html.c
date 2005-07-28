#include "asf.h"
#include "asf_meta.h"
#include "geolocate.h"
#include "dateUtil.h"

#define VERSION 1.0

void usage(char *name)
{
  printf("\n"
         "USAGE:\n"
         "   %s  <list>\n",name);
  printf("\n"
         "REQUIRED ARGUMENTS:\n"
         "   list   List of baseline files.");
  printf("\n\n"
         "DESCRIPTION:\n"
         "   %s converts baseline information into HTML format.\n", name);
  printf("\n"
         "Version %.2f, ASF SAR Tools\n"
         "\n",VERSION);
  exit(EXIT_FAILURE);
}

int main(int argc, char *argv[])
{
  char file[255]="", inFile[255], outFile[255], txtFile[255], shpFile[255], *listFile;
  char buffer[1000], mode[10], time1[30], time2[10000][30], bla[25], direction;
  int frame[10000], orbit1, orbit2[10000], Bp[10000], Bn[10000], dt[10000];
  int sequence1[10000], sequence2[10000], ref_frame[1000];
  float lat[10000], lon[10000], lat1[1000], lon1[1000], lat2[1000], lon2[1000];
  float lat3[1000], lon3[1000], lat4[1000], lon4[1000];
  int rev, i, j, k, n, m;
  FILE *fpIn, *fpOut, *fpList, *fpTxt, *fpMon, *fpShape;
  julian_date jd;
  hms_time hms;
  ymd_date ymd, ymd2[10000];
  char *mon[13]={"", "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};
  char *month[13]={"", "January", "February", "March", "April", "May", "June", 
                    "July", "August", "September", "October", "November", "December"};

  quietflag=1;
  
  /* Parse command line args */
  if ((argc-currArg) < 1) {
    printf("Insufficient arguments.\n"); 
    usage(argv[0]);
  }

  listFile = argv[1];
  fpList = FOPEN(listFile, "r");  
  fpMon = FOPEN("month.lst", "w");
  while (fgets(buffer, 1000, fpList)) {
    strncpy(file, buffer, 9);
    sprintf(inFile, "%s.out", file);
    sprintf(outFile, "%s.html", file);
    sprintf(txtFile, "%s.txt", file);
    sprintf(shpFile, "%s_shape.in", file);
    fpIn = FOPEN(inFile, "r");
    fpOut = FOPEN(outFile, "w");
    fpTxt = FOPEN(txtFile, "w");
    fpShape = FOPEN(shpFile, "r");

    /* Read corner coordinates out of the shape parser file */
    m=0;
    while (fgets(buffer, 1000, fpShape)) {
      sscanf(buffer, "%s %s %s %i %s %f %f %f %f %f %f %f %f", bla, bla, bla, &ref_frame[m], bla, 
	     &lat1[m], &lon1[m], &lat2[m], &lon2[m], &lat3[m], &lon3[m], &lat4[m], &lon4[m]);
      m++;
    }
    
    /* Read first line to get beam mode, orbit number and time stamp */
    fgets(buffer, 1000, fpIn);
    sscanf(buffer, "%s\t%d\t%d\t%d\t%s\t%d\t%d\t%s\t%i\t%i\t%d\t%f\t%f", 
	   mode, &(frame[0]), &orbit1, &(sequence1[0]), time1, &(orbit2[0]), 
	   &(sequence2[0]), time2[0], &(Bp[0]), &(Bn[0]), &(dt[0]), 
	   &(lat[0]), &(lon[0]));
    sscanf(time1, "%4d-%3dT%2d:%2d:%lf", 
	   &jd.year, &jd.jd, &hms.hour, &hms.min, &hms.sec);
    date_jd2ymd(&jd, &ymd);
    sscanf(time2[0], "%4d-%3dT%2d:%2d:%lf", 
	   &jd.year, &jd.jd, &hms.hour, &hms.min, &hms.sec);
    date_jd2ymd(&jd, &ymd2[0]);
    rev = orbit2[0];
    n=1;
    fprintf(fpMon, "%d\t%s\t%d\n", orbit1, month[ymd.month], ymd.year);

    /* Write HTML header for baseline information */
    fprintf(fpOut, "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01"
	    " Transitional//EN\">\n");
    fprintf(fpOut, "<html>\n<head>\n");
    fprintf(fpOut, "<title>RADARSAT %s %d baselines</title>\n", mode, orbit1);
    fprintf(fpOut, "<meta http-equiv=\"Content-Type\""
	    " content=\"text/html; charset=iso-8859-1\">\n");
    fprintf(fpOut, "</head>\n<body>\n");
    fprintf(fpOut, "<strong>Satellite:</strong> R1<br>\n"
	    "<strong>Beam mode:</strong> %s<br>\n"
	    "<strong>Sequence:</strong> %d<br>\n"
	    "<strong>Date:</strong> %d-%s-%d<br>\n"
	    "<strong>Reference orbit:</strong> %d<br><br>\n",
	    mode, sequence1[0], ymd.day, mon[ymd.month], ymd.year, orbit1);
    if (orbit1 >= 10000) {
      fprintf(fpOut, "<a href=\"%s_%d.txt\">Text version</a><br>\n", mode, orbit1);
      fprintf(fpOut, "<a href=\"%s_%d.tar.gz\">Downloadable shape file in GIS format</a><br><br>\n", mode, orbit1);
    }
    else {
      fprintf(fpOut, "<a href=\"%s_0%d.txt\">Text version</a><br>\n", mode, orbit1);
      fprintf(fpOut, "<a href=\"%s_0%d.tar.gz\">Downloadable shape file in GIS format</a><br><br>\n", mode, orbit1);
    }
    fprintf(fpOut, "<table border=1 cellpadding=5>\n");
    fprintf(fpOut, "<tr><td align=center width=60><strong>Orbit<br>No.</strong></td>\n"
	    "<td align=center width=60><strong>Sequence</strong></td>\n"
	    "<td align=center width=60><strong>Date</strong></td>\n"
	    "<td align=center width=60><strong>Frame<br>No.</strong></td>\n"
	    "<td align=center width=60><strong>||<br>[m]</strong></td>\n"
	    "<td align=center width=60><strong>_|_<br>[m]</strong></td>\n"
	    "<td align=center width=60><strong>&Delta;t<br>[days]</strong></td>\n"
	    "<td align=center width=80><strong>Center Lat<br>[deg]</strong></td>\n"
	    "<td align=center width=80><strong>Center Lon<br>[deg]</strong></td></tr>\n");

    /* Read rest of baseline file */
    while (fgets(buffer, 1000, fpIn)) {
      sscanf(buffer, "%s\t%d\t%d\t%d\t%s\t%d\t%d\t%s\t%i\t%i\t%d\t%f\t%f", 
	     mode, &(frame[n]), &orbit1, &(sequence1[n]), time1, &(orbit2[n]), 
	     &(sequence2[n]), time2[n], &(Bp[n]), &(Bn[n]), &(dt[n]), 
	     &(lat[n]), &(lon[n]));
      sscanf(time2[n], "%4d-%3dT%2d:%2d:%lf", 
 	     &jd.year, &jd.jd, &hms.hour, &hms.min, &hms.sec);
      date_jd2ymd(&jd, &ymd2[n]);
      n++;
    }
    for (i=0; i<n; i++) {
      fprintf(fpOut, "<tr><td valign=top>%5d</td>\n", orbit2[i]);
      fprintf(fpOut, "<td valign=top>%2d</td>\n", sequence2[i]);
      fprintf(fpOut, "<td valign=top>%d-%s-%d</td>\n<td>", 
              ymd2[i].day, mon[ymd2[i].month], ymd2[i].year);
      for (j=i; j<n; j++) {
        if (rev == orbit2[j])
	  fprintf(fpOut, "%3d<br>", frame[j]);
        else
          break;
      }
      fprintf(fpOut, "</td>\n<td>");
      for (j=i; j<n; j++) {
        if (rev == orbit2[j])
          fprintf(fpOut, "%i<br>", Bp[j]);
        else
          break;
      }
      fprintf(fpOut, "</td>\n<td>");
      for (j=i; j<n; j++) {
        if (rev == orbit2[j])
          fprintf(fpOut, "%i<br>", Bn[j]);
        else
          break;
      }
      fprintf(fpOut, "</td>\n<td>");
      for (j=i; j<n; j++) {
        if (rev == orbit2[j])
          fprintf(fpOut, "%i<br>", dt[j]);
        else
          break;
      }
      fprintf(fpOut, "</td>\n<td>");
      for (j=i; j<n; j++) {
        if (rev == orbit2[j])
          fprintf(fpOut, "%.4f<br>", lat[j]);
        else
          break;
      }
      fprintf(fpOut, "</td>\n<td>");
      for (j=i; j<n; j++) {
        if (rev == orbit2[j])
          fprintf(fpOut, "%.4f<br>", lon[j]);
        else
          break;
      }
      fprintf(fpOut, "</td></tr>\n");
      for (j=i; j<n; j++) {
        if (rev == orbit2[j]) {
	  for (k=0; k<m; k++) {
	    if (frame[j] == ref_frame[k]) {
              if (frame[j]>=225 && frame[j]<=675) direction = 'D';
              else direction = 'A';
	      fprintf(fpTxt, "%s\t%d\t%c\t%d\t%d\t%s\t%d\t%d\t%s\t%i\t%i\t%d"
		      "\t%.4f\t%.4f\t%.4f\t%.4f\t%.4f\t%.4f\t%.4f\t%.4f\t%.4f\t%.4f\n", 
		      mode, frame[j], direction, orbit1, sequence1[j], time1, orbit2[j], 
		      sequence2[j], time2[j], Bp[j], Bn[j], dt[j], lat[j], lon[j],
		      lat1[k], lon1[k], lat2[k], lon2[k], lat3[k], lon3[k], lat4[k], lon4[k]);
	    }
	  }
	}
	else
	  break;
      }
      rev = orbit2[j];
      i = j-1;
    }

    /* Write HTML footer for baseline information */
    fprintf(fpOut, "</table>\n");
    fprintf(fpOut, "<br><br>\n<a href=\"%s%d.html\">Back to %s %d baselines</a>"
	    "<br><br>\n", month[ymd.month], ymd.year, month[ymd.month], ymd.year);
    fprintf(fpOut, "<hr>\n");
    fprintf(fpOut, "<font size=1>\nCopyright &copy; 2004, ");
    fprintf(fpOut, "<A HREF=\"mailto:uso@asf.alaska.edu\">"
	    "Alaska Satellite Facility</a><br>\n");
    fprintf(fpOut, "Created on %s<br>\n", date_stamp());
    fprintf(fpOut,"</font>\n</body>\n</html>\n\n");
    

    FCLOSE(fpIn);
    FCLOSE(fpOut);
    FCLOSE(fpTxt);
    FCLOSE(fpShape);

  }
  FCLOSE(fpList);
  FCLOSE(fpMon);
  return(0);
}
