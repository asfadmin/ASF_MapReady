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
  char file[255]="", inFile[255], outFile[255], txtFile[255], areaFile[255], *listFile;
  char buffer[1000], cmd[255], mode[10], dir[5000], bla[25];
  char lat_str[25], lon_str[25], area_dir[25], dir_str[20], area_str[10];
  float lat[5000], lon[5000];
  float lat_low, lat_high, lon_low, lon_high;
  int frame[5000], area, asc[10], desc[10], i, j, k, n, num_asc, num_desc, first=1;
  FILE *fpOut, *fpList, *fpTxt, *fpShp, *fpArea;
  quietflag=1;
  
  /* Parse command line args */
  if ((argc-currArg) < 1) {
    printf("Insufficient arguments.\n"); 
    usage(argv[0]);
  }

  listFile = argv[1];
  fpList = FOPEN(listFile, "r");

  /* Go through list of pass information for each baseline text file */  
  while (fgets(buffer, 1000, fpList)) {
    strncpy(file, buffer, 9);
    sprintf(txtFile, "%s.txt", file);

    /* Read the information out the text file */
    fpTxt = FOPEN(txtFile, "r");
    n=0;
    while (fgets(buffer, 1000, fpTxt)) {
      sscanf(buffer, "%s %i %c %s %s %s %s %s %s %s %s %s %f %f",
	     mode, &frame[n], &dir[n], bla, bla, bla, bla, bla, bla, bla, bla, bla,
	     &lat[n], &lon[n]);
      n++;
    }
    FCLOSE(fpTxt);

    /* Create shape file and database file */
    if (first) {
      sprintf(cmd, "shpcreate %s_asc_coverage polygon", mode);
      system(cmd);
      sprintf(cmd, "dbfcreate %s_asc_coverage.dbf -n area 5 0", mode);
      system(cmd);
      sprintf(cmd, "shpcreate %s_desc_coverage polygon", mode);
      system(cmd);
      sprintf(cmd, "dbfcreate %s_desc_coverage.dbf -n area 5 0", mode);
      system(cmd);
      first = 0;
    }
    /* Check out the center coordinates and assign the baseline file to an area file */
    sprintf(areaFile, "tmp_area.lst");
    fpArea = FOPEN(areaFile, "a");
    num_asc=0;
    num_desc=0;
    for (i=0; i<10; i++) {
      asc[i] = 0; /* just for initialization */
      desc[i] = 0;
    }
    for (i=0; i<n; i++) {
      for (j=0; i<72; i++)
	for (k=0; k<36; k++) {
	  lat_high = 90.0 - k*5.0;
	  lat_low = lat_high - 5.0;
	  lon_low = -180.0 + i*5.0;
	  lon_high = lon_low + 5;
	  area = k*72 + i + 1;
	  if (lat[i]>lat_low && lat[i]<lat_high && lon[i]>lon_low && lon[i]<lon_high) {
	    if (dir[i] == 'A' && area!=asc[num_asc])
	      asc[num_asc++] = area;
	    if (dir[i] == 'D' && area!=desc[num_desc])
	      desc[num_desc++] = area;
	  }
	}
    }
    for (i=0; i<num_asc; i++) {
      if (asc[i]<10)
	sprintf(outFile, "area_000%i_asc.lst", asc[i]);
      else if (asc[i]>=10 && asc[i]<100)
	sprintf(outFile, "area_00%i_asc.lst", asc[i]);
      else if (asc[i]>=100 && asc[i]<1000)
	sprintf(outFile, "area_0%i_asc.lst", asc[i]);
      else
	sprintf(outFile, "area_%i_asc.lst", asc[i]);
      outFile[17] = '\0';
      fpOut = FOPEN(outFile, "a");
      fprintf(fpOut, "%s\n", file);
      FCLOSE(fpOut);
      fprintf(fpArea, "%s\n", outFile);
    }
    for (i=0; i<num_desc; i++) {
      if (desc[i]<10)
	sprintf(outFile, "area_000%i_desc.lst", desc[i]);
      else if (desc[i]>=10 && desc[i]<100)
	sprintf(outFile, "area_00%i_desc.lst", desc[i]);
      else if (desc[i]>=100 && desc[i]<1000)
	sprintf(outFile, "area_0%i_desc.lst", desc[i]);
      else
	sprintf(outFile, "area_%i_desc.lst", desc[i]);
      outFile[18] = '\0';
      fpOut = FOPEN(outFile, "a");
      fprintf(fpOut, "%s\n", file);
      FCLOSE(fpOut);
      fprintf(fpArea, "%s\n", outFile);
    }
    FCLOSE(fpArea);
  }
  FCLOSE(fpList);
  sprintf(cmd, "cat tmp_area.lst | sort -u > %s_area.lst", mode);
  system(cmd);
  sprintf(areaFile, "%s_area.lst", mode);

  /* Create 'no baselines available' web pages for all areas. 
     Web pages for areas with data will be created later and overwrite these ones. */
  for (i=0; i<2; i++) {
    if (i==0) 
      sprintf(dir_str, "Ascending");
    else 
      sprintf(dir_str, "Descending");
    for (j=0; j<72; j++)
      for (k=0; k<36; k++) {
	lat_high = 90.0 - k*5.0;
	lat_low = lat_high - 5.0;
	lon_low = -180.0 + j*5.0;
	lon_high = lon_low + 5;
	area = k*72 + j + 1;
	if (area<10 && i==0) {
	  sprintf(outFile, "area_000%i_asc.html", area);
	  sprintf(area_str, "000%i", area);
	}
	else if (area>=10 && area<100 && i==0) {
	  sprintf(outFile, "area_00%i_asc.html", area);
	  sprintf(area_str, "00%i", area);
	}
	else if (area>=100 && area<1000 && i==0) {
	  sprintf(outFile, "area_0%i_asc.html", area);
	  sprintf(area_str, "0%i", area);
	}
	else if (i==0) {
	  sprintf(outFile, "area_%i_asc.html", area);
	  sprintf(area_str, "%i", area);
	}
	if (i==0) outFile[18] = '\0';
	if (area<10 && i==1) {
	  sprintf(outFile, "area_000%i_desc.html", area);
	  sprintf(area_str, "000%i", area);
	}
	else if (area>=10 && area<100 && i==1) {
	  sprintf(outFile, "area_00%i_desc.html", area);
	  sprintf(area_str, "00%i", area);
	}
	else if (area>=100 && area<1000 && i==1) {
	  sprintf(outFile, "area_0%i_desc.html", area);
	  sprintf(area_str, "0%i", area);
	}
	else if (i==1) {
	  sprintf(outFile, "area_%i_desc.html", area);
	  sprintf(area_str, "%i", area);
	}
	if (i==1) outFile[19] = '\0';
	
	fpOut = FOPEN(outFile, "w");
	fprintf(fpOut, "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01"
		" Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">\n");
	fprintf(fpOut, "<html>\n<head>\n");
	fprintf(fpOut, "<title>ASF Radarsat-1 interferometric baseline catalog - Search results</title>\n");
	fprintf(fpOut, "<meta http-equiv=\"Content-Type\""
		" content=\"text/html; charset=iso-8859-1\">\n");
	fprintf(fpOut, "</head>\n<body bgcolor=\"#FFFFFF\">\n");
	fprintf(fpOut, "<img src=\"../img/frontmidlogo.gif\" alt=\"ASF logo\" width=\"150\" height=\"88\">\n");
	fprintf(fpOut, "<table width=\"700\">\n");
	fprintf(fpOut, "<tr><td width=\"35\"></td>\n");
	fprintf(fpOut, "<td colspan=\"3\">");
	fprintf(fpOut, "<h2>Radarsat-1 interferometric baseline catalog - Search results</h2></td></tr>\n");
	fprintf(fpOut, "<tr><td></td>\n");
	fprintf(fpOut, "<td><img src=\"../img/worldmap_%s.jpg\" height=\"139\" width=\"139\" ", area_str);
	if (lat_low<0)
	  sprintf(lat_str, "%.0fS to %.0fS", fabs(lat_high), fabs(lat_low));
	else
	  sprintf(lat_str, "%.0fN to %.0fN", lat_low, lat_high);
	if (lon_low<0)
	  sprintf(lon_str, "%.0fW to %.0fW", fabs(lon_high), fabs(lon_low));
	else
	  sprintf(lon_str, "%.0fE to %.0fE", lon_low, lon_high); 
	fprintf(fpOut, "border=\"1\" alt=\"Area located Lat: %s, Lon: %s\"></td>\n", lat_str, lon_str);
	fprintf(fpOut, "<td><strong>Beam mode:</strong> %s<br>\n", mode);
	fprintf(fpOut, "<strong>Orbit direction:</strong> %s<br>\n", dir_str);
	if (lat_low<0)
	  sprintf(lat_str, "%.0f&deg;S to %.0f&deg;S", fabs(lat_high), fabs(lat_low));
	else
	  sprintf(lat_str, "%.0f&deg;N to %.0f&deg;N", lat_low, lat_high);
	if (lon_low<0)
	  sprintf(lon_str, "%.0f&deg;W to %.0f&deg;W", fabs(lon_high), fabs(lon_low));
	else
	  sprintf(lon_str, "%.0f&deg;E to %.0f&deg;E", lon_low, lon_high); 
	fprintf(fpOut, "<strong>Latitude:</strong> %s<br>\n", lat_str);
	fprintf(fpOut, "<strong>Longitude:</strong> %s<br><br>\n", lon_str);
	fprintf(fpOut, "No baselines available for this area<br>in the ASF archive\n");
	fprintf(fpOut, "</td><td width=\"265\"></td></tr>\n");
	fprintf(fpOut, "<tr><td></td><td colspan=\"3\">\n");
	fprintf(fpOut, "<br><a href=\"../index.html\">Back to geographic baseline search</a><br><br><hr>\n");
	fprintf(fpOut, "<font size=1>\n");
	fprintf(fpOut, "Copyright &copy; 2005, <a HREF=\"mailto:uso@asf.alaska.edu\">");
	fprintf(fpOut, "Alaska Satellite Facility</a><br>\n");
	fprintf(fpOut, "Compiled by Rudi Gens<br>\n");
	fprintf(fpOut, "Created on %s<br>\n", date_stamp());
	fprintf(fpOut,"</font>\n</body>\n</html>\n\n");
	FCLOSE(fpOut);
      }
  }
    
  /* Go through the area file, bundle text and shape files up, and create web pages */
  fpArea = FOPEN(areaFile, "r");
  while (fgets(buffer, 1000, fpArea)) {
    if (buffer[10] == 'a') {
      strncpy(txtFile, buffer, 17);
      txtFile[17] = '\0';
      strncpy(area_dir, txtFile, 13);
      area_dir[13] = '\0';
    }
    else if (buffer[10] == 'd') {
      strncpy(txtFile, buffer, 18);
      txtFile[18] = '\0';
      strncpy(area_dir, txtFile, 14);
      area_dir[14] = '\0';
    }

    /* Tar and compress baseline text files for area */
    fpTxt = FOPEN("text_tar.lst", "w");
    fpList = FOPEN(txtFile, "r");
    while (fgets(buffer, 1000, fpList)) {
      strncpy(inFile, buffer, 9);
      inFile[9] = '\0';
      fprintf(fpTxt, "%s.txt\n", inFile);
    }
    FCLOSE(fpTxt);
    FCLOSE(fpList);
    sprintf(cmd, "tar --create --file=%s_txt.tar -T text_tar.lst", area_dir);
    system(cmd);
    sprintf(cmd, "gzip -f %s_txt.tar", area_dir);
    system(cmd);

    /* Tar and compress baseline shape files for area */
    fpShp = FOPEN("shape_tar.lst", "w");
    fpList = FOPEN(txtFile, "r");
    while (fgets(buffer, 1000, fpList)) {
      strncpy(inFile, buffer, 9);
      inFile[9] = '\0';
      fprintf(fpShp, "%s.shp\n%s.dbf\n%s.shx\n%s.shp.xml\n", inFile, inFile, inFile, inFile);
    }
    FCLOSE(fpShp);
    FCLOSE(fpList);
    sprintf(cmd, "tar --create --file=%s_shp.tar -T shape_tar.lst", area_dir);
    system(cmd);
    sprintf(cmd, "gzip -f %s_shp.tar", area_dir);
    system(cmd);

    /* Create HTML page for area file */
    sscanf(area_dir, "area_%i_", &area);
    strncpy(area_str, &area_dir[5], 4);
    area_str[5] = '\0';
    area = atoi(area_str);
    if (area_dir[10]=='a')
      sprintf(dir_str, "Ascending");
    if (area_dir[10]=='d')
      sprintf(dir_str, "Descending");
    k = (area - 1)/72;
    i = area - 1 - k*72;
    lat_high = 90.0 - k*5.0;
    lat_low = lat_high - 5.0;
    lon_low = -180.0 + i*5.0;
    lon_high = lon_low + 5;

    sprintf(outFile, "%s.html", area_dir);
    outFile[strlen(area_dir)+5] = '\0';
    fpOut = FOPEN(outFile, "w");
    fprintf(fpOut, "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01"
	    " Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">\n");
    fprintf(fpOut, "<html>\n<head>\n");
    fprintf(fpOut, "<title>ASF Radarsat-1 interferometric baseline catalog - Search results</title>\n");
    fprintf(fpOut, "<meta http-equiv=\"Content-Type\""
	    " content=\"text/html; charset=iso-8859-1\">\n");
    fprintf(fpOut, "</head>\n<body bgcolor=\"#FFFFFF\">\n");
    fprintf(fpOut, "<img src=\"../img/frontmidlogo.gif\" alt=\"ASF logo\" width=\"150\" height=\"88\">\n");
    fprintf(fpOut, "<table width=\"700\">\n");
    fprintf(fpOut, "<tr><td width=\"35\"></td>\n");
    fprintf(fpOut, "<td colspan=\"3\">");
    fprintf(fpOut, "<h2>Radarsat-1 interferometric baseline catalog - Search results</h2></td></tr>\n");
    fprintf(fpOut, "<tr><td></td>\n");
    fprintf(fpOut, "<td><img src=\"../img/worldmap_%s.jpg\" height=\"139\" width=\"139\" ", area_str);
    if (lat_low<0)
      sprintf(lat_str, "%.0fS to %.0fS", fabs(lat_high), fabs(lat_low));
    else
      sprintf(lat_str, "%.0fN to %.0fN", lat_low, lat_high);
    if (lon_low<0)
      sprintf(lon_str, "%.0fW to %.0fW", fabs(lon_high), fabs(lon_low));
    else
      sprintf(lon_str, "%.0fE to %.0fE", lon_low, lon_high); 
    fprintf(fpOut, "border=\"1\" alt=\"Area located Lat: %s, Lon: %s\"></td>\n", lat_str, lon_str);
    fprintf(fpOut, "<td><strong>Beam mode:</strong> %s<br>\n", mode);
    fprintf(fpOut, "<strong>Orbit direction:</strong> %s<br>\n", dir_str);
    if (lat_low<0)
      sprintf(lat_str, "%.0f&deg;S to %.0f&deg;S", fabs(lat_high), fabs(lat_low));
    else
      sprintf(lat_str, "%.0f&deg;N to %.0f&deg;N", lat_low, lat_high);
    if (lon_low<0)
      sprintf(lon_str, "%.0f&deg;W to %.0f&deg;W", fabs(lon_high), fabs(lon_low));
    else
      sprintf(lon_str, "%.0f&deg;E to %.0f&deg;E", lon_low, lon_high); 
    fprintf(fpOut, "<strong>Latitude:</strong> %s<br>\n", lat_str);
    fprintf(fpOut, "<strong>Longitude:</strong> %s<br><br>\n", lon_str);
    fprintf(fpOut, "<a href=\"%s_txt.tar.gz\">Text version of baselines</a><br>\n", area_dir);
    fprintf(fpOut, "<a href=\"%s_shp.tar.gz\">Shape file version of baselines</a>\n", area_dir);
    fprintf(fpOut, "</td><td width=\"265\"></td></tr>\n");
    fprintf(fpOut, "<tr><td></td><td colspan=\"3\">\n");
    fprintf(fpOut, "<br><a href=\"../index.html\">Back to geographic baseline search</a><br><br><hr>\n");
    fprintf(fpOut, "<font size=1>\n");
    fprintf(fpOut, "Copyright &copy; 2005, <a HREF=\"mailto:uso@asf.alaska.edu\">");
    fprintf(fpOut, "Alaska Satellite Facility</a><br>\n");
    fprintf(fpOut, "Compiled by Rudi Gens<br>\n");
    fprintf(fpOut, "Created on %s<br>\n", date_stamp());
    fprintf(fpOut,"</font>\n</body>\n</html>\n\n");
    FCLOSE(fpOut);

    /* Add shape file for coverage map */
    if (area_dir[10]=='a') {
      sprintf(cmd, "shpadd %s_asc_coverage %9.4f %9.4f %9.4f %9.4f "
	      "%9.4f %9.4f %9.4f %9.4f %9.4f %9.4f", 
	      mode, lon_low, lat_low, lon_low, lat_high, 
	      lon_high, lat_high, lon_high, lat_low, lon_low, lat_low);
      system(cmd);
      sprintf(cmd, "dbfadd %s_asc_coverage.dbf %i", mode, area);
      system(cmd);
    }
    if (area_dir[10]=='d') {
      sprintf(cmd, "shpadd %s_desc_coverage %9.4f %9.4f %9.4f %9.4f "
	      "%9.4f %9.4f %9.4f %9.4f %9.4f %9.4f", 
	      mode, lon_low, lat_low, lon_low, lat_high, 
	      lon_high, lat_high, lon_high, lat_low, lon_low, lat_low);
      system(cmd);
      sprintf(cmd, "dbfadd %s_desc_coverage.dbf %i", mode, area);
      system(cmd);
    }
  }
  FCLOSE(fpList);
  FCLOSE(fpArea);

  return(0);
}
