#include "asf_vector.h"
#include "asf.h"
#include "dateUtil.h"
#include <assert.h>
#include <ctype.h>

static void strip_begin_whitesp_comment_inplace(char *s)
{
    char *p = s;

    // skip a # only if it is the first character
    if (*p=='#')
      ++p;
    // now skip whitespace
    while (isspace(*p))
      ++p;
    // now move characters starting at p back to s.
    char *q = s;
    while (*p != '\0') {
      *q = *p;
      ++p; ++q;
    }
    *q = '\0';
}

static char *strip_end_whitesp(const char *s)
{
    char *ret = STRDUP(s);
    char *p = ret + strlen(ret) - 1;
    while (isspace(*p) && p>ret)
        *p-- = '\0';
    return ret;
}

static int is_lat_column(const char *name)
{
  if (strncmp_case(name,"lat",3)==0 ||
      strcmp_case(name,"scn_lulat")==0 ||
      strcmp_case(name,"scn_rulat")==0 ||
      strcmp_case(name,"scn_ldlat")==0 ||
      strcmp_case(name,"scn_rdlat")==0 ||
      strcmp_case(name,"lulat")==0 ||
      strcmp_case(name,"ldlat")==0 ||
      strcmp_case(name,"rulat")==0 ||
      strcmp_case(name,"rdlat")==0 ||
      strcmp_case(name,"ullat")==0 ||
      strcmp_case(name,"lllat")==0 ||
      strcmp_case(name,"lrlat")==0 ||
      strcmp_case(name,"urlat")==0 ||
      strcmp_case(name,"Near Start Lat")==0 ||
      strcmp_case(name,"Far Start Lat")==0 ||
      strcmp_case(name,"Near End Lat")==0 ||
      strcmp_case(name,"Far End Lat")==0 ||
      strcmp_case(name,"NSTART_LAT")==0 ||
      strcmp_case(name,"FSTART_LAT")==0 ||
      strcmp_case(name,"N_END_LAT")==0 ||
      strcmp_case(name,"F_END_LAT")==0)
  {
    return TRUE;
  }
  return FALSE;
}

static int is_lon_column(const char *name)
{
  if (strncmp_case(name,"lon",3)==0 ||
      strcmp_case(name,"scn_lulon")==0 ||
      strcmp_case(name,"scn_rulon")==0 ||
      strcmp_case(name,"scn_ldlon")==0 ||
      strcmp_case(name,"scn_rdlon")==0 ||
      strcmp_case(name,"lulon")==0 ||
      strcmp_case(name,"ldlon")==0 ||
      strcmp_case(name,"rulon")==0 ||
      strcmp_case(name,"rdlon")==0 ||
      strcmp_case(name,"ullon")==0 ||
      strcmp_case(name,"lllon")==0 ||
      strcmp_case(name,"lrlon")==0 ||
      strcmp_case(name,"urlon")==0 ||
      strcmp_case(name,"Near Start Lon")==0 ||
      strcmp_case(name,"Far Start Lon")==0 ||
      strcmp_case(name,"Near End Lon")==0 ||
      strcmp_case(name,"Far End Lon")==0 ||
      strcmp_case(name,"NSTART_LON")==0 ||
      strcmp_case(name,"FSTART_LON")==0 ||
      strcmp_case(name,"N_END_LON")==0 ||
      strcmp_case(name,"F_END_LON")==0)
  {
    return TRUE;
  }
  return FALSE;
}

static int lineSegmentsIntersect(
    double Ax, double Ay,
    double Bx, double By,
    double Cx, double Cy,
    double Dx, double Dy)
{
  double  distAB, theCos, theSin, newX, ABpos ;

  //  Fail if either line segment is zero-length.
  if ((Ax==Bx && Ay==By) || (Cx==Dx && Cy==Dy)) return FALSE;

  //  (1) Translate the system so that point A is on the origin.
  Bx-=Ax; By-=Ay;
  Cx-=Ax; Cy-=Ay;
  Dx-=Ax; Dy-=Ay;

  //  Discover the length of segment A-B.
  distAB=sqrt(Bx*Bx+By*By);

  //  (2) Rotate the system so that point B is on the positive X axis.
  theCos=Bx/distAB;
  theSin=By/distAB;
  newX=Cx*theCos+Cy*theSin;
  Cy  =Cy*theCos-Cx*theSin; Cx=newX;
  newX=Dx*theCos+Dy*theSin;
  Dy  =Dy*theCos-Dx*theSin; Dx=newX;

  //  Fail if segment C-D doesn't cross line A-B.
  if ((Cy<0. && Dy<0.) || (Cy>=0. && Dy>=0.)) return FALSE;

  //  (3) Discover the position of the intersection point along line A-B.
  ABpos=Dx+(Cx-Dx)*Dy/(Dy-Cy);

  //  Fail if segment C-D crosses line A-B outside of segment A-B.
  if (ABpos<0. || ABpos>distAB) return FALSE;

  //  Success.
  return TRUE;
}

FILE *csv_open(const char *filename,
               int *num_meta_cols, csv_meta_column_t **meta_column_info,
               int *num_data_cols, csv_data_column_t **data_column_info)
{
  // first: initialize the output poitners to NULL, so if we return early,
  // (due to an error), the outputs are all properly empty
  *num_meta_cols = *num_data_cols = 0;
  *meta_column_info = NULL;
  *data_column_info = NULL;

  // try to open the csv file
  char *csv_filename=NULL;
  FILE *fp = fopen(filename, "r");
  if (!fp) {
    // failed to open, try adding ".csv" extension
    csv_filename = appendExt(filename, ".csv");
    fp = fopen(csv_filename, "r");

    if (!fp)
      asfPrintError("Couldn't open: %s\n", filename);
  }
  else
    csv_filename = STRDUP(filename);
  assert(fp);

  // now get the header line
  char line[1024];
  if (!fgets(line, 1023, fp)) {
    asfPrintStatus("File %s: Empty.\n", filename);
    FCLOSE(fp);
    return NULL;
  }

  strip_end_whitesp_inplace(line);
  strip_begin_whitesp_comment_inplace(line);

  // go through the columns
  // first time through, we are just counting columns
  char *p = line;
  int n=0, n_lat_cols=0, n_lon_cols=0;

  do {
    char col[256];
    p = quoted_string_parse(p,col,256,1,',');
    ++n;

    if (is_lat_column(col))
      ++n_lat_cols;
    else if (is_lon_column(col))
      ++n_lon_cols;
  } while (p);

  int n_data_cols = n_lat_cols+n_lon_cols;
  int n_meta_cols = n;

  printf("Found %d columns (%d lat, %d lon).\n", n, n_lat_cols, n_lon_cols);

  if (n_lat_cols != n_lon_cols) {
    printf("Don't have an equal number of lat and lon columns!\n");
    printf("Cannot ingest this file.\n");
    FCLOSE(fp);
    return NULL;
  }
  if (n_lat_cols == 0 || n_lon_cols == 0) {
    printf("No data columns found.\n");
    printf("Cannot ingest this file.\n");
    FCLOSE(fp);
    return NULL;
  }

  // now can allocate the proper amount of storage
  csv_meta_column_t *meta_inf = MALLOC(sizeof(csv_meta_column_t)*n_meta_cols);
  csv_data_column_t *data_inf = MALLOC(sizeof(csv_data_column_t)*n_data_cols);

  // second pass: populate the info
  p = line;

  int i,j;
  int meta_index=0, data_index=0, lat_index=0, lon_index=0;

  for (i=0; i<n; ++i) {
    char col[64];
    p = quoted_string_parse(p,col,64,-1,',');
    
    if (is_lat_column(col)) {
      assert(data_index < n_data_cols);
      strncpy_safe(data_inf[data_index].column_name, col, 64);
      data_inf[data_index].column_number = i;
      data_inf[data_index].is_lat = TRUE;

      ++lat_index;
      ++data_index;
    }
    else if (is_lon_column(col)) {
      assert(data_index < n_data_cols);
      strncpy_safe(data_inf[data_index].column_name, col, 64);
      data_inf[data_index].column_number = i;
      data_inf[data_index].is_lat = FALSE;

      ++lon_index;
      ++data_index;
    }

    assert(meta_index < n_meta_cols);
    strncpy_safe(meta_inf[meta_index].column_name, col, 64);
    meta_inf[meta_index].column_number = i;
    meta_inf[meta_index].data_type = CSV_UNKNOWN;

    ++meta_index;
  }
  assert(p==NULL); // we read the last column, should have gotten a NULL
  assert(lat_index == n_lat_cols);
  assert(lon_index == n_lon_cols);
  assert(meta_index == n_meta_cols);
  assert(data_index == lat_index+lon_index);

  // now-- parse the rest of the file, trying to find out what type of
  // data each column contains
  int *non_number_flags = CALLOC(sizeof(int),n);
  int *non_integer_flags = CALLOC(sizeof(int),n);
  int *non_bool_flags = CALLOC(sizeof(int),n);

  int line_num=2;
  while (fgets(line,1023,fp)) {
    // go through the metadata columns
    strip_end_whitesp_inplace(line);
    for (i=0; i<n_meta_cols; ++i) {
      int col = meta_inf[i].column_number;
      const char *val = get_str(line, col);

      if (strlen(val)>1)
        non_bool_flags[col] = TRUE;
      if (strcmp(val,"-")==0) {
        non_integer_flags[col] = TRUE;
        non_number_flags[col] = TRUE;
      }

      for (j=0; j<strlen(val); ++j) {

        if (!isdigit(val[j]) && val[j] != '+' && val[j] != '-')
          non_integer_flags[col] = TRUE;
        if (val[j] != '-' && val[j] != '+' && j>0)
          non_integer_flags[col] = TRUE;

        if (val[j] != '0' && val[j] != '1' && 
            val[j] != 'Y' && val[j] != 'y' &&
            val[j] != 'N' && val[j] != 'n')
          non_bool_flags[col] = TRUE;
        
        if (!isdigit(val[j]) && val[j] != '.' &&
            val[j] != 'E' && val[j] != 'e' &&
            val[j] != '+' && val[j] != '-')
          non_number_flags[col] = TRUE;
      }
    }
    ++line_num;
  }
  FCLOSE(fp);
  
  // now can figure out each columns data type
  for (i=0; i<n_meta_cols; ++i) {
    int col = meta_inf[i].column_number;
    if (!non_bool_flags[col])
      meta_inf[i].data_type = CSV_LOGICAL;
    else if (non_number_flags[col])
      meta_inf[i].data_type = CSV_STRING;
    else if (non_integer_flags[col])
      meta_inf[i].data_type = CSV_DOUBLE;
    else
      meta_inf[i].data_type = CSV_INTEGER;
  }
  
  // open up the file again, skip header line
  fp = FOPEN(csv_filename, "r");
  fgets(line, 1023, fp);
  
  // last thing: set the output pointers
  *num_meta_cols = n_meta_cols;
  assert(n_data_cols%2==0);
  *num_data_cols = n_data_cols/2;
  *meta_column_info = meta_inf;
  *data_column_info = data_inf;
  return fp;
}

static const char *csv_type_str(int data_type)
{
  switch (data_type) {
    default:
    case CSV_UNKNOWN: return "Unknown";
    case CSV_STRING:  return "String ";
    case CSV_DOUBLE:  return "Double ";
    case CSV_INTEGER: return "Integer";
    case CSV_LOGICAL: return "Logical";
    case CSV_DATE:    return "Date   ";
  }
}

void csv_info(int num_meta_cols, csv_meta_column_t *meta_column_info,
              int num_data_cols, csv_data_column_t *data_column_info)
{
  int i;
  asfPrintStatus("CSV File Information (%d columns)\n", num_meta_cols);

  asfPrintStatus("\nMetadata: (%d column%s)\n", num_meta_cols,
                 num_meta_cols==1?"":"s");
  for (i=0; i<num_meta_cols; ++i) {
    int col = meta_column_info[i].column_number;
    asfPrintStatus("Column %2d: %s %s\n", col,
                   csv_type_str(meta_column_info[i].data_type),
                   meta_column_info[i].column_name);
  }

  asfPrintStatus("\nLat/Lon Data: (%d points)\n",num_data_cols);
  int n_lat = 0, n_lon = 0;
  for (i=0; i<num_data_cols*2; ++i) {
    int col = data_column_info[i].column_number;
    if (data_column_info[i].is_lat) {
      ++n_lat;
      asfPrintStatus("Column %2d: LAT %2d, %s\n", col,
                     n_lat, data_column_info[i].column_name);
    }
    else {
      ++n_lon;
      asfPrintStatus("Column %2d: LON %2d, %s\n", col,
                     n_lon, data_column_info[i].column_name);
    }
  }
  assert(n_lat==n_lon);
  assert(n_lat+n_lon==num_data_cols*2);

  asfPrintStatus("\n");
}

int csv_line_parse(const char *line_in, int line_num,
                   int num_meta_cols, csv_meta_column_t *meta_column_info,
                   int num_data_cols, csv_data_column_t *data_column_info,
                   char ***column_data_o, double **lats_o, double **lons_o)
{
  *column_data_o = NULL;
  *lats_o = *lons_o = NULL;

  char *line = strip_end_whitesp(line_in);

  // ignore blank lines, and comment lines
  if (strlen(line)==0 || line[0]=='#')
    return FALSE;
    
  int i;
  int n_lat = 0, n_lon = 0;

  double *lats = MALLOC(sizeof(double)*num_data_cols);
  double *lons = MALLOC(sizeof(double)*num_data_cols);

  for (i=0; i<num_data_cols*2; ++i) {
    int col = data_column_info[i].column_number;
    const char *data = get_str(line, col);
    if (!data)
      return FALSE;

    if (data_column_info[i].is_lat) {
      lats[n_lat] = atof(data);
      // verify valid latitude
      if (lats[n_lat] < -90 || lats[n_lat] > 90) {
        asfPrintStatus("Invalid latitude on line %d, column %s: %f\n",
                       line_num, data_column_info[i].column_name, lats[n_lat]);
        FREE(lats); FREE(lons);
        return FALSE;
      }
      ++n_lat;
    }
    else {
      lons[n_lon] = atof(data);
      // verify valid longitude
      if (lons[n_lon] < -360 || lons[n_lon] > 360) {
        asfPrintStatus("Invalid longitude on line %d, column %s: %f\n",
                       line_num, data_column_info[i].column_name, lons[n_lon]);
        FREE(lats); FREE(lons);
        return FALSE;
      }
      // longitude should be -180 to 180
      if (lons[n_lon] < -180) lons[n_lon] += 360;
      if (lons[n_lon] > 180) lons[n_lon] -= 360;
      ++n_lon;
    }
  }

  char **column_data = MALLOC(sizeof(char*)*num_meta_cols);

  for (i=0; i<num_meta_cols; ++i) {
    column_data[i] = (char*)MALLOC(sizeof(char)*256);
    int col = meta_column_info[i].column_number;
    const char *val = get_str(line, col);
    if (!val)
      return FALSE;

    strncpy_safe(column_data[i], val, 64);
  }
  FREE(line);

  // it is often the case that for rectangular regions, the points are
  // not specified in a clockwise, or counterclockwise, manner.  We can
  // detect this and fix it.
  if (num_data_cols==4) {
    if (lineSegmentsIntersect(lats[1],lons[1],
                              lats[2],lons[2],
                              lats[0],lons[0],
                              lats[3],lons[3]))
    {
      // swap 2 & 3...
      double tmp = lats[2];
      lats[2] = lats[3];
      lats[3] = tmp;

      tmp = lons[2];
      lons[2] = lons[3];
      lons[3] = tmp;
    }
  }

  *column_data_o = column_data;
  *lats_o = lats;
  *lons_o = lons;

  return TRUE;
}

void csv_free(int num_meta_cols, char **column_data,
              double *lats, double *lons)
{
  int i;
  for (i=0; i<num_meta_cols; ++i)
    FREE(column_data[i]);
  FREE(column_data);
  FREE(lats);
  FREE(lons);
}

void csv_dump(const char *filename)
{
  int num_meta_cols, num_data_cols;
  csv_meta_column_t *meta_column_info;
  csv_data_column_t *data_column_info;

  FILE *fp = csv_open(filename,
                      &num_meta_cols, &meta_column_info,
                      &num_data_cols, &data_column_info);

  // csv_open() returns NULL if the file can't be processed
  if (!fp)
    return;

  csv_info(num_meta_cols, meta_column_info, num_data_cols, data_column_info);

  // start line counter at 1 (header line is not part of this loop)
  int line_num=1;

  char line[1024];
  while (fgets(line, 1023, fp)) {
    ++line_num;

    char **column_data;
    double *lats, *lons;
    int ok = csv_line_parse(line, line_num,
                            num_meta_cols, meta_column_info,
                            num_data_cols, data_column_info,
                            &column_data, &lats, &lons);

    // csv_line_parse() will return FALSE when the line is invalid,
    // or should be skipped for whatever reason (e.g., comment line)
    if (!ok)
      continue;

    asfPrintStatus("Line #%d\n", line_num);
    int i;

    // dealing with metadata
    asfPrintStatus("Metadata... (%d items)\n", num_meta_cols);
    for (i=0; i<num_meta_cols; ++i) {

      char *val = column_data[i];
      char *name = meta_column_info[i].column_name;

      switch (meta_column_info[i].data_type) {
        case CSV_STRING:
          {
            // write out attribute "name" with string value "val"
            asfPrintStatus("%s (str): %s\n", name, val);
          }
          break;
          
        case CSV_DOUBLE:
          {
            double d = atof(val);
            // write out attribute "name" with double value "d"
            asfPrintStatus("%s (double): %f\n", name, d);
          }
          break;
          
        case CSV_INTEGER:
          {
            int k = atoi(val);
            // write out attribute "name" with integer value "k"
            asfPrintStatus("%s (int): %d\n", name, k);
          }
          break;

        case CSV_LOGICAL:
          {
            int b = atoi(val);
            // write out attribute "name" with boolean value "b" (0 or 1)
            // you could combine this with the integer case as well
            asfPrintStatus("%s (bool): %s\n", name, b?"YES":"NO");
          }
          break;

        case CSV_DATE:
          {
            // not yet implemented!
            asfPrintStatus("%s (date): %s\n", name, val);
          }
          break;

        case CSV_UNKNOWN:
          {
            // should never happen
            asfPrintStatus("%s (unknown): %s\n", name, val);
          }
          break;
      }
    }

    // deal with the point (lat/lon) data
    asfPrintStatus("\nData... (%d points)\n", num_data_cols);
    for (i=0; i<num_data_cols; ++i) {

      double lat = lats[i];
      double lon = lons[i];

      // write out point #i
      // note polygon is not necessarily closed, depends on client data
      asfPrintStatus("Point #%2d: %16.7f %16.7f\n", i, lat, lon);
    }

    csv_free(num_meta_cols, column_data, lats, lons);

    asfPrintStatus("\n");
  }
  FCLOSE(fp);
}

// Convert generic csv to kml file
int csv2kml(const char *in_file, const char *out_file, int listFlag)
{
  int num_meta_cols, num_data_cols;
  csv_meta_column_t *meta_column_info;
  csv_data_column_t *data_column_info;

  FILE *ifp = csv_open(in_file,
                      &num_meta_cols, &meta_column_info,
                      &num_data_cols, &data_column_info);

  // csv_open() returns NULL if the file can't be processed
  if (!ifp)
    return FALSE;

  FILE *ofp = FOPEN(out_file, "w");
  if (!ofp) {
    printf("Failed to open output file %s: %s\n", out_file, strerror(errno));
    return FALSE;
  }

  kml_header(ofp);

  // this is just for debugging, if you want to print out what was found
  csv_info(num_meta_cols, meta_column_info, num_data_cols, data_column_info);

  // start line counter at 1 (header line is not part of this loop)
  int i,line_num=1;

  char line[1024];
  while (fgets(line, 1023, ifp)) {
    ++line_num;

    char **column_data;
    double *lats, *lons;
    int ok = csv_line_parse(line, line_num,
                            num_meta_cols, meta_column_info,
                            num_data_cols, data_column_info,
                            &column_data, &lats, &lons);

    // csv_line_parse() will return FALSE when the line is invalid
    if (!ok)
      continue;

    // try to get a good column for this...
    char name_to_show[64];
    strcpy(name_to_show, "");

    fprintf(ofp, "<Placemark>\n");
    fprintf(ofp, "  <description><![CDATA[\n");
    fprintf(ofp, "<!-- Format: CSV (generated by convert2vector "
	    "(version %s)) -->\n", SVN_REV);

    // dealing with metadata
    for (i=0; i<num_meta_cols; ++i) {

      char *val = column_data[i];
      char *name = meta_column_info[i].column_name;

      switch (meta_column_info[i].data_type) {
        case CSV_STRING:
          {
            // write out attribute "name" with string value "val"
            fprintf(ofp,"<strong>%s</strong>: %s<br>\n", name, val);
            if (strcmp_case(name,"ID")==0 ||
                strcmp_case(name,"NAME")==0 ||
                strcmp_case(name,"SCNID")==0 ||
                strstr(name,"_ID")!=NULL)
              strcpy(name_to_show,val);
          }
          break;
          
        case CSV_DOUBLE:
          {
            double d = atof(val);
            // write out attribute "name" with double value "d"
            fprintf(ofp,"<strong>%s</strong>: %f<br>\n", name, d);
          }
          break;
          
        case CSV_INTEGER:
          {
            int k = atoi(val);
            // write out attribute "name" with integer value "k"
            fprintf(ofp,"<strong>%s</strong>: %d<br>\n", name, k);
          }
          break;

        case CSV_LOGICAL:
          {
            int b = atoi(val);
            // write out attribute "name" with boolean value "b" (0 or 1)
            // you could combine this with the integer case as well
            fprintf(ofp,"<strong>%s</strong>: %s<br>\n", name, b?"YES":"NO");
          }
          break;

        default:
        case CSV_UNKNOWN:
        case CSV_DATE:
          {
            // should never happen
            fprintf(ofp,"<strong>%s</strong>: %s<br>\n", name, val);
          }
          break;
      }
    }

    // if we couldn't figure out an "ID" or "NAME" column, show first col
    if (strlen(name_to_show)==0)
      strcpy(name_to_show,column_data[0]);

    double clon=0,clat=0;
    for (i=0; i<num_data_cols; ++i) {
      clat += lats[i];
      clon += lons[i];
    }
    clon /= (double)num_data_cols;
    clat /= (double)num_data_cols;

    fprintf(ofp, "  ]]></description>\n");
    fprintf(ofp, "  <name>%s</name>\n", name_to_show);
    fprintf(ofp, "  <LookAt>\n");
    fprintf(ofp, "    <longitude>%.10f</longitude>\n", clon);
    fprintf(ofp, "    <latitude>%.10f</latitude>\n", clat);
    fprintf(ofp, "    <range>400000</range>\n");
    //fprintf(ofp, "    <tilt>30</tilt>\n");
    fprintf(ofp, "  </LookAt>\n");
    fprintf(ofp, "  <visibility>1</visibility>\n");
    fprintf(ofp, "  <open>1</open>\n");

    write_kml_style_keys(ofp);

    if (num_data_cols > 1) {
      fprintf(ofp, "  <Polygon>\n");
      fprintf(ofp, "    <extrude>1</extrude>\n");
      fprintf(ofp, "    <altitudeMode>%s</altitudeMode>\n", altitude_mode());
      fprintf(ofp, "    <outerBoundaryIs>\n");
      fprintf(ofp, "     <LinearRing>\n");
      fprintf(ofp, "      <coordinates>\n");
      
      // deal with the point (lat/lon) data
      for (i=0; i<num_data_cols; ++i)
	fprintf(ofp, "       %.12f,%.12f,7000\n", lons[i],lats[i]);
      
      // close polygon
      fprintf(ofp, "       %.12f,%.12f,7000\n", lons[0],lats[0]);
      
      fprintf(ofp, "      </coordinates>\n");
      fprintf(ofp, "     </LinearRing>\n");
      fprintf(ofp, "    </outerBoundaryIs>\n");
      fprintf(ofp, "  </Polygon>\n");
    }
    else {
      fprintf(ofp, "  <Point>\n");
      fprintf(ofp, "    <coordinates>%f,%f,0</coordinates>\n", 
	      lons[0], lats[0]);
      fprintf(ofp, "  </Point>\n");
    }
    fprintf(ofp, "</Placemark>\n");

    csv_free(num_meta_cols, column_data, lats, lons);
  }
  FCLOSE(ifp);
  kml_footer(ofp);
  FCLOSE(ofp);
  return TRUE;
}

char *fix_attribute_name(const char *name)
{
  int len = strlen(name);
  if (len>10) len=10; // truncate to 10 characters

  char *ret = CALLOC(sizeof(char),len+1);

  int i;
  for (i=0; i<len; ++i) {
    // allowed characters are any except: space, period, comma
    if (isspace(name[i]) || name[i]=='.' || name[i]==',')
      ret[i]='_';
    else
      ret[i]=name[i];
  }

  if (strcmp(ret,name)!=0)
    asfPrintStatus("Attribute name changed: %s -> %s\n", name, ret);

  return ret;
}

// Initialize shape file
void shape_csv_init(char *inFile, csv_meta_column_t *meta_column_info,
		    int num_meta_cols, int num_data_cols)
{
  DBFHandle dbase;
  SHPHandle shape;

  // Initialize output
  char *dbaseFile = appendExt(inFile, ".dbf");
  dbase = DBFCreate(dbaseFile);
  if (!dbase)
    asfPrintError("Could not create database file '%s'\n", dbaseFile);

  int i;
  for (i=0; i<num_meta_cols; i++) {
    char *name = fix_attribute_name(meta_column_info[i].column_name);
    switch (meta_column_info[i].data_type)
      {
      case CSV_STRING:
      case CSV_DATE:
        if (DBFAddField(dbase, name, FTString, 50, 0) == -1)
          asfPrintError("Could not add %s field to database file\n",
			meta_column_info[i].column_name);
        break;
      case CSV_DOUBLE:
        if (DBFAddField(dbase, name, FTDouble, 16, 7) == -1)
          asfPrintError("Could not add %s field to database file\n",
			meta_column_info[i].column_name);
        break;
      case CSV_INTEGER:
      case CSV_LOGICAL:
        if (DBFAddField(dbase, name, FTInteger,15, 0) == -1)
          asfPrintError("Could not add %s field to database file\n",
			meta_column_info[i].column_name);
        break;
      default:
        asfPrintWarning("DBF column type not supported!\n");
        break;
      }
    FREE(name);
  }

  // Close the database for initialization
  DBFClose(dbase);

  // Open shapefile for initialization
  char *shpfile = appendExt(inFile, ".shp");
  if (num_data_cols == 1)
    shape = SHPCreate(shpfile, SHPT_POINT);
  else if (num_data_cols > 1)
    shape = SHPCreate(shpfile, SHPT_POLYGON);
  else
    asfPrintError("No geolocation information in the input file.\n");

  // Close shapefile for initialization
  SHPClose(shape);
  FREE(dbaseFile);
}

// Convert generic csv to shapefile
int csv2shape(char *inFile, char *outFile, int listFlag)
{
  int num_meta_cols, num_data_cols;
  csv_meta_column_t *meta_column_info;
  csv_data_column_t *data_column_info;

  FILE *fp = csv_open(inFile,
                      &num_meta_cols, &meta_column_info,
                      &num_data_cols, &data_column_info);

  // csv_open() returns NULL if the file can't be processed
  if (!fp)
    return FALSE;

  // this is just for debugging, if you want to print out what was found
  csv_info(num_meta_cols, meta_column_info, num_data_cols, data_column_info);

  // start line counter at 1 (header line is not part of this loop)
  int line_num=1;
  int entry_num=0;

  // shapefile stuff
  DBFHandle dbase;
  SHPHandle shape;

  shape_csv_init(outFile, meta_column_info, num_meta_cols, num_data_cols);

  char *shpfile = appendExt(outFile, ".shp");
  open_shape(shpfile, &dbase, &shape);

  // these store the output lat/lon
  double *write_lon = MALLOC(sizeof(double)*(num_data_cols+1));
  double *write_lat = MALLOC(sizeof(double)*(num_data_cols+1));

  char line[1024];
  while (fgets(line, 1023, fp)) {
    ++line_num;

    char **column_data;
    double *lats, *lons;
    int ok = csv_line_parse(line, line_num,
                            num_meta_cols, meta_column_info,
                            num_data_cols, data_column_info,
                            &column_data, &lats, &lons);

    // csv_line_parse() will return FALSE when the line is invalid
    if (!ok)
      continue;

    // dealing with metadata
    int i;
    for (i=0; i<num_meta_cols; ++i) {

      char *val = column_data[i];
      char *name = meta_column_info[i].column_name;

      switch (meta_column_info[i].data_type) {
        case CSV_STRING:
        case CSV_DATE:
          DBFWriteStringAttribute(dbase, entry_num, i, val);
          break;

        case CSV_DOUBLE:
          DBFWriteDoubleAttribute(dbase, entry_num, i, atof(val));
          break;

        case CSV_INTEGER:
        case CSV_LOGICAL:
          DBFWriteIntegerAttribute(dbase, entry_num, i, atoi(val));
          break;

        case CSV_UNKNOWN:
          // should never happen
          asfPrintStatus("%s (unknown): %s\n", name, val);
          break;
      }
    }

    // Write shape object
    SHPObject *shapeObject=NULL;
    if (num_data_cols == 1) {
      shapeObject =
        SHPCreateSimpleObject(SHPT_POINT, 1, &lons[0], &lats[0], NULL);
    }
    else {
      for (i=0; i<num_data_cols; ++i) {
        write_lat[i] = lats[i];
        write_lon[i] = lons[i];
      }
      write_lat[num_data_cols]=write_lat[0]; // closing the polygon
      write_lon[num_data_cols]=write_lon[0];
      shapeObject =
        SHPCreateSimpleObject(SHPT_POLYGON, num_data_cols+1,
                              write_lon, write_lat, NULL);
    }
    SHPWriteObject(shape, -1, shapeObject);
    SHPDestroyObject(shapeObject);

    csv_free(num_meta_cols, column_data, lats, lons);

    ++entry_num;
  }
  FCLOSE(fp);
  FREE(write_lon);
  FREE(write_lat);
  FREE(shpfile);

  // Clean up
  close_shape(dbase, shape);
  write_esri_proj_file(outFile);

  return TRUE;
}
