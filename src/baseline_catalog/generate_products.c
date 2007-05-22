#include "asf_baseline.h"

// Prototypes
void init_baseline_shape(char *inFile);
void open_baseline_shape(char *inFile, DBFHandle *dbase, SHPHandle *shape);
void close_baseline_shape(DBFHandle dbase, SHPHandle shape);

void generate_products(struct base_pair *pairs, int nPairs)
{
  DBFHandle dbase=NULL;
  SHPHandle shape=NULL;
  FILE *fpDB=NULL, *fpText=NULL, *fpKml=NULL;
  char dbFile[255], textFile[255], kmlFile[255], shapeFile[255]; 
  int i, old_orbit=-99;

  for (i=0; i<nPairs; i++) {    
    if (pairs[i].master != old_orbit) {
      // Closing open files if necessary
      if (fpDB)
	FCLOSE(fpDB);
      if (fpText)
	FCLOSE(fpText);
      if (fpKml) {
	kml_footer(fpKml);
	FCLOSE(fpKml);
      }
      if (dbase && shape)
	close_baseline_shape(dbase, shape);

      // Open database file and write header
      sprintf(dbFile, "%s_%s_%d_%d.db", pairs[i].sensor, pairs[i].mode, 
	      pairs[i].track, pairs[i].master);
      fpDB = FOPEN(dbFile, "wt");
      fprintf(fpDB, "Sensor\t");
      fprintf(fpDB, "Beam_mode\t");
      fprintf(fpDB, "Track\t");
      fprintf(fpDB, "Frame\t");
      fprintf(fpDB, "Master\t");
      fprintf(fpDB, "Master_seq\t");
      fprintf(fpDB, "Slave\t");
      fprintf(fpDB, "Slave_seq\t");
      fprintf(fpDB, "B_par\t");
      fprintf(fpDB, "B_perp\t");
      fprintf(fpDB, "B_temp\n");

      // Open baseline text file and write header
      sprintf(textFile, "%s_%s_%d.txt", 
	      pairs[i].sensor, pairs[i].mode, pairs[i].master);
      fpText = FOPEN(textFile, "wt");
      fprintf(fpText, "Sensor\t");
      fprintf(fpText, "Beam_mode\t");
      fprintf(fpText, "Frame\t");
      fprintf(fpText, "Orbit_dir\t");
      fprintf(fpText, "Master\t");
      fprintf(fpText, "Master_seq\t");
      fprintf(fpText, "Master_time\t");
      fprintf(fpText, "Slave\t");
      fprintf(fpText, "Slave_seq\t");
      fprintf(fpText, "Slave_time\t");
      fprintf(fpText, "B_par\t");
      fprintf(fpText, "B_perp\t");
      fprintf(fpText, "B_temp\t");
      fprintf(fpText, "Center_lat\t");
      fprintf(fpText, "Center_lon\t");
      fprintf(fpText, "NS_lat\t");
      fprintf(fpText, "NS_lon\t");
      fprintf(fpText, "FS_lat\t");
      fprintf(fpText, "FS_lon\t");
      fprintf(fpText, "NE_lat\t");
      fprintf(fpText, "NE_lon\t");
      fprintf(fpText, "FE_lat\t");
      fprintf(fpText, "FE_lon\n");

      // Write baseline KML file
      sprintf(kmlFile, "%s_%s_%d.kml", 
	      pairs[i].sensor, pairs[i].mode, pairs[i].master);
      fpKml = FOPEN(kmlFile, "wt");
      kml_header(fpKml);

      // Write baseline shape file
      sprintf(shapeFile, "%s_%s_%d",
	      pairs[i].sensor, pairs[i].mode, pairs[i].master);
      init_baseline_shape(shapeFile);
      open_baseline_shape(shapeFile, &dbase, &shape);
    }

    // Write into database file
    fprintf(fpDB, "%s\t", pairs[i].sensor);
    fprintf(fpDB, "%s\t", pairs[i].mode);
    fprintf(fpDB, "%d\t", pairs[i].track);
    fprintf(fpDB, "%d\t", pairs[i].frame);
    fprintf(fpDB, "%d\t", pairs[i].master);
    fprintf(fpDB, "%d\t", pairs[i].m_seq);
    fprintf(fpDB, "%d\t", pairs[i].slave);
    fprintf(fpDB, "%d\t", pairs[i].s_seq);
    fprintf(fpDB, "%d\t", pairs[i].b_par);
    fprintf(fpDB, "%d\t", pairs[i].b_perp);
    fprintf(fpDB, "%d\n", pairs[i].b_temp);

    // Write into baseline text file
    fprintf(fpText, "%s\t", pairs[i].sensor);
    fprintf(fpText, "%s\t", pairs[i].mode);
    fprintf(fpText, "%d\t", pairs[i].frame);
    fprintf(fpText, "%s\t", pairs[i].orbit_dir);
    fprintf(fpText, "%d\t", pairs[i].master);
    fprintf(fpText, "%d\t", pairs[i].m_seq);
    fprintf(fpText, "%s\t", pairs[i].m_time);
    fprintf(fpText, "%d\t", pairs[i].slave);
    fprintf(fpText, "%d\t", pairs[i].s_seq);
    fprintf(fpText, "%s\t", pairs[i].s_time);
    fprintf(fpText, "%d\t", pairs[i].b_par);
    fprintf(fpText, "%d\t", pairs[i].b_perp);
    fprintf(fpText, "%d\t", pairs[i].b_temp);
    fprintf(fpText, "%.4f\t", pairs[i].c_lat);
    fprintf(fpText, "%.4f\t", pairs[i].c_lon);
    fprintf(fpText, "%.4f\t", pairs[i].ns_lat);
    fprintf(fpText, "%.4f\t", pairs[i].ns_lon);
    fprintf(fpText, "%.4f\t", pairs[i].fs_lat);
    fprintf(fpText, "%.4f\t", pairs[i].fs_lon);
    fprintf(fpText, "%.4f\t", pairs[i].ne_lat);
    fprintf(fpText, "%.4f\t", pairs[i].ne_lon);
    fprintf(fpText, "%.4f\t", pairs[i].fe_lat);
    fprintf(fpText, "%.4f\n", pairs[i].fe_lon);

    // Write into baseline kml file
    baseline2kml(i, pairs, fpKml);

    // Write into baseline shape file
    baseline2shape(i, pairs, dbase, shape);

  }
  FCLOSE(fpDB);
  FCLOSE(fpText);
  kml_footer(fpKml);
  FCLOSE(fpKml);
  close_baseline_shape(dbase, shape);
}
