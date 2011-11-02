#include <unistd.h>
#include <strings.h>
#include <ctype.h>

#include "asf_convert_gui.h"
#include "ceos_thumbnail.h"
#include "asf.h"
#include "get_ceos_names.h"

int COL_INPUT_FILE;
int COL_INPUT_FILE_SHORT;
int COL_ANCILLARY_FILE;
int COL_METADATA_FILE;
int COL_ALL_AUX_FILES;
int COL_ALL_AUX_FILES_SHORT;
int COL_INPUT_THUMBNAIL;
int COL_BAND_LIST;
int COL_OUTPUT_FILE;
int COL_OUTPUT_FILE_SHORT;
int COL_STATUS;
int COL_LOG;
int COL_POLSARPRO_INFO;
int COL_POLSARPRO_DISPLAY;
int COL_INTERFEROGRAM;
int COL_COHERENCE;
int COL_SLAVE_METADATA;
int COL_BASELINE;

int COMP_COL_INPUT_FILE;
int COMP_COL_INPUT_FILE_SHORT;
int COMP_COL_ANCILLARY_FILE;
int COMP_COL_ORIGINAL_METADATA_FILE;
int COMP_COL_OUTPUT_FILE;
int COMP_COL_OUTPUT_FILE_SHORT;
int COMP_COL_OUTPUT_THUMBNAIL;
int COMP_COL_OUTPUT_THUMBNAIL_BIG;
int COMP_COL_STATUS;
int COMP_COL_LOG;
int COMP_COL_TMP_DIR;
int COMP_COL_LAYOVER_SHADOW_MASK_FILE;
int COMP_COL_CLIPPED_DEM_FILE;
int COMP_COL_SIMULATED_SAR_FILE;
int COMP_COL_FARADAY_FILE;
int COMP_COL_HIST_FILE;
int COMP_COL_CLASS_MAP_FILE;
int COMP_COL_METADATA_FILE;
int COMP_COL_POLSARPRO_INFO;
int COMP_COL_INTERFEROGRAM;
int COMP_COL_COHERENCE;
int COMP_COL_SLAVE_METADATA;
int COMP_COL_BASELINE;
int COMP_COL_INCID_ANGLES_FILE;

gboolean move_to_files_list(const gchar *data_file,
                            const gchar *ancillary_file,
                            const gchar *meta_file,
                            const gchar *polsarpro_aux_info,
			    const gchar *interferogram_file,
			    const gchar *coherence_file,
			    const gchar *slave_metadata_file,
			    const gchar *baseline_file);

/* Returns true if any of the input files in the input files list */
/* are the type that need an ancillary file, i.e. gamma and polsarpro */
gboolean have_ancillary_files_in_list()
{
  gboolean have_ancillary_files = FALSE;

  // For each file in the input list, check to see if it is a type
  // that requires an ancillary file
  GtkTreeIter iter;
  gboolean more_items =
      gtk_tree_model_get_iter_first(GTK_TREE_MODEL(list_store), &iter);
  while (more_items) {
    gchar * input_file;
    gtk_tree_model_get(GTK_TREE_MODEL(list_store), &iter,
                       COL_INPUT_FILE, &input_file, -1);
    if (input_file && is_polsarpro(input_file)) {
      have_ancillary_files = TRUE;
    }
    g_free(input_file);
    if (have_ancillary_files) {
      break;
    }
    more_items = gtk_tree_model_iter_next(GTK_TREE_MODEL(list_store), &iter);
  }

  return have_ancillary_files;
}

/* Returns true if any of the input files in the input files list */
/* are polsarpro files */
gboolean have_polsarpro_files_in_list()
{
  gboolean have_polsarpro_files = FALSE;

  GtkTreeIter iter;
  gboolean more_items =
      gtk_tree_model_get_iter_first(GTK_TREE_MODEL(list_store), &iter);
  while (more_items) {
    gchar * input_file;
    gtk_tree_model_get(GTK_TREE_MODEL(list_store), &iter,
                       COL_INPUT_FILE, &input_file, -1);
    if (input_file && is_polsarpro(input_file)) {
      have_polsarpro_files = TRUE;
    }
    g_free(input_file);
    if (have_polsarpro_files) {
      break;
    }
    more_items = gtk_tree_model_iter_next(GTK_TREE_MODEL(list_store), &iter);
  }

  return have_polsarpro_files;
}

/* Returns true if any of the input files in the input files list */
/* are the type that need an ancillary file, i.e. gamma and polsarpro */
gboolean have_meta_files_in_list()
{
  gboolean have_meta_files = FALSE;

  // For each file in the input list, check to see if it is a type
  // that requires an ancillary file
  GtkTreeIter iter;
  gboolean more_items =
      gtk_tree_model_get_iter_first(GTK_TREE_MODEL(list_store), &iter);
  while (more_items) {
    gchar * meta_file;
    gtk_tree_model_get(GTK_TREE_MODEL(list_store), &iter,
                       COL_METADATA_FILE, &meta_file, -1);
    if (meta_file && strlen(meta_file)>0) {
      have_meta_files = TRUE;
    }
    g_free(meta_file);
    if (have_meta_files) {
      break;
    }
    more_items = gtk_tree_model_iter_next(GTK_TREE_MODEL(list_store), &iter);
  }

  return have_meta_files;
}

/* Returns the length of the prepension if there is an allowed
   prepension, otherwise returns 0 (no prepension -> chceck extensions) */
int has_prepension(const gchar * data_file_name)
{
    /* at the moment, the only prepension we allow is LED- (ALOS) */
    char *basename = get_basename(data_file_name);
    int ret = strncmp(basename, "LED-", 4) == 0;
    free(basename);
    return ret ? 4 : 0;
}

static gchar *
determine_default_output_file_name(const gchar * data_file_name)
{
    return determine_default_output_file_name_schemed(data_file_name,
                                                      current_naming_scheme);
}

gboolean is_meta_file(const gchar * data_file)
{
  char *ext = findExt(data_file);
  if (ext && strcmp_case(ext, ".meta")==0)
    return TRUE;

  char *basename = MALLOC(sizeof(char)*(strlen(data_file)+10));
  char **dataName = NULL, **metaName = NULL;
  int i, nBands, trailer, ret=FALSE;

  ceos_file_pairs_t s = get_ceos_names(data_file, basename,
                            &dataName, &metaName, &nBands, &trailer);

  // Check for raw Palsar, and AVNIR or PRISM level 1A or 1B1 data
  ceos_description *ceos = NULL;
  if (s != NO_CEOS_FILE_PAIR) {
      ceos = get_ceos_description(data_file, REPORT_LEVEL_NONE);
  }

  if (s != NO_CEOS_FILE_PAIR &&
      s != CEOS_RAW_LDR_PAIR &&
      s != CEOS_raw_ldr_PAIR &&
      !((ceos->sensor == PRISM || ceos->sensor == AVNIR) &&
        (ceos->product == LEVEL_1A || ceos->product == LEVEL_1B1)) &&
      (ceos && ceos->product != RAW))
  {
      for (i=0; i<nBands; ++i) {
          if (strcmp(data_file, metaName[i])==0) {
              ret = TRUE;
          }
      }
  }

  FREE(basename);
  free_ceos_names(dataName, metaName);

  return ret;
}

static char *file_is_valid(const gchar * file)
{
    // is the file name a valid string?
    if (!file || (file && strlen(file) < 1)) return NULL;

    // first, check if the file is ASF Internal
    char *ext = findExt(file);
    if (ext && strcmp_case(ext, ".meta")==0) {
        // Don't support complex types when importing via the GUI
        meta_parameters *meta = meta_read(file);
        if (meta->general->data_type == COMPLEX_BYTE ||
            meta->general->data_type == COMPLEX_INTEGER16 ||
            meta->general->data_type == COMPLEX_INTEGER32 ||
            meta->general->data_type == COMPLEX_REAL32 ||
            meta->general->data_type == COMPLEX_REAL64)
        {
            meta_free(meta);
            return NULL;
        }
        meta_free(meta);
        return STRDUP(file);
    }
    else if (ext && strcmp_case(ext, ".img")==0) {
        // Don't support complex types when importing via the GUI
        meta_parameters *meta = meta_read(file);
        if (meta->general->data_type == COMPLEX_BYTE ||
            meta->general->data_type == COMPLEX_INTEGER16 ||
            meta->general->data_type == COMPLEX_INTEGER32 ||
            meta->general->data_type == COMPLEX_REAL32 ||
            meta->general->data_type == COMPLEX_REAL64)
        {
            meta_free(meta);
            return NULL;
        }
        meta_free(meta);
        return appendExt(file, ".meta");
    }

    // second possiblity: geotiff
    // (we don't actually check that it is a geotiff (instead of a tiff)
    // it'll error out during import if it isn't)
    if (ext && (strcmp_case(ext, ".tif")==0 || strcmp_case(ext, ".tiff")==0))
        return STRDUP(file);

    // third possibility: airsar
    if (ext && (strcmp_case(ext, ".airsar")==0))
        return STRDUP(file);

    // fourth possibility: PolSARpro (.bin and .bin.hdr)
    if (ext && (strcmp_case(ext, ".bin")==0))
        return STRDUP(file);
    if (ext && (strcmp_case(ext, ".hdr")==0)) {
      char *dupe = STRDUP(file);
      ext = findExt(dupe);
      *ext = '\0';
      ext = findExt(dupe);
      if (ext && (strcmp_case(ext, ".bin")==0)) {
        return dupe;
      }
      FREE(dupe);
    }
    if (is_polsarpro(file) && !ext) {
      char *_file = (char *)MALLOC(sizeof(char)*(strlen(file)+5));
      sprintf(_file, "%s.bin", file);
      return _file;
    }

    // allow xml files to be added -- Terrasar-x, Radarsat-2
    if (ext && strcmp_case(ext, ".xml")==0)
      return STRDUP(file);

    // check for ROI_PAC files
    // FIXME: amplitude image for the moment - make sure that the amplitude
    // file name is returned in any case
    if (ext && strcmp_case(ext, ".rsc")==0)
      return STRDUP(file);

    // check for ALOS mosaics - might have an extension (or not)
    // so we check for a little more
    if (endsWith(file, "_HDR.txt") || endsWith(file, "_HDR")) {
      char *dupe = STRDUP(file);
      char *p = strstr(dupe, "_HDR");
      *p = '\0';
      return dupe;
    }

    // now, the ceos check
    char *basename = MALLOC(sizeof(char)*(strlen(file)+10));
    char **dataName = NULL, **metaName = NULL;
    int nBands, trailer;

    ceos_file_pairs_t ret = get_ceos_names(file, basename,
                                &dataName, &metaName, &nBands, &trailer);

    FREE(basename);

    // Check for raw Palsar, and AVNIR or PRISM level 1A or 1B1 data
    ceos_description *ceos = NULL;
    if (ret != NO_CEOS_FILE_PAIR) {
        ceos = get_ceos_description(file, REPORT_LEVEL_NONE);
    }

    if (ret != NO_CEOS_FILE_PAIR &&
        ret != CEOS_RAW_LDR_PAIR &&
        ret != CEOS_raw_ldr_PAIR &&
        !((ceos->sensor == PRISM || ceos->sensor == AVNIR) &&
          (ceos->product == LEVEL_1A || ceos->product == LEVEL_1B1)) &&
        (ceos && ceos->product != RAW))
    {
        // Found -- return metadata file
        char *meta_file=NULL;
        int i;
        if (ret != CEOS_IMG_LED_PAIR) {
            for (i=0; i<nBands; ++i) {
                if (strcmp(file, metaName[i])==0) {
                    meta_file = STRDUP(metaName[i]);
                    break;
                }
            }
        }

        if (!meta_file) {
            meta_file = STRDUP(metaName[0]);
        }

        free_ceos_names(dataName, metaName);
        FREE(ceos);

        return meta_file;
    } else {
        // not found
        if (ret != NO_CEOS_FILE_PAIR) {
            free_ceos_names(dataName, metaName);
        }
        FREE(ceos);
        return NULL;
    }
}

#ifdef THUMBNAILS

static void set_input_image_thumbnail(GtkTreeIter *iter,
                                      const gchar *metadata_file,
                                      gchar *data_file,
                                      gchar *ancillary_file,
                                      const char *lut_basename)
{
    GdkPixbuf *pb = make_input_image_thumbnail_pixbuf (
      metadata_file, data_file, lut_basename, THUMB_SIZE);

    if (pb) {
        gtk_list_store_set (list_store, iter, COL_INPUT_THUMBNAIL, pb, -1);
    }
    else {
        // failed to generate a thumbnail from the data file -- if this is
        // gamma data, this failure is to be expected, we'll generate one
        // from the ancillary (CEOS) data
        if (strlen(ancillary_file) > 0) {
          pb = make_input_image_thumbnail_pixbuf (
            ancillary_file, ancillary_file, lut_basename, THUMB_SIZE);

          if (pb)
            gtk_list_store_set (list_store, iter, COL_INPUT_THUMBNAIL, pb, -1);
        }
    }
}

static void
do_thumbnail (const gchar *file)
{
    gchar *metadata_file = meta_file_name (file);
    gchar *data_file = data_file_name (file);

    // Forcing of CEOS thumbnails when available
    if (is_polsarpro(data_file)) {
      if(strlen(metadata_file) > 0) {
        g_free(data_file);
        data_file = g_strdup(metadata_file);
      }
      else {
	g_free(metadata_file);
	metadata_file = (gchar*)g_malloc(sizeof(gchar)*(strlen(data_file) + 5));
	sprintf(metadata_file, "%s%s", data_file, ".hdr");
      }
      if (!fileExists(metadata_file)) strcpy(metadata_file, "");
    }

        /* Find the element of the list store having the file name we are
           trying to add a thumbnail of.  */
        GtkTreeIter iter;
        gboolean valid;
        /* Get the first iter in the list */
        valid = gtk_tree_model_get_iter_first (GTK_TREE_MODEL (list_store),
                                               &iter);
        while ( valid ) {
            /* Walk through the list, reading each row */
            gchar *input_file;
            gchar *ancillary_file;
            gchar *polsarpro_aux_info;
            gtk_tree_model_get (GTK_TREE_MODEL (list_store), &iter,
                                COL_INPUT_FILE, &input_file,
                                COL_ANCILLARY_FILE, &ancillary_file,
                                COL_POLSARPRO_INFO, &polsarpro_aux_info,
                                -1);
            if ( (metadata_file && strcmp (metadata_file, input_file) == 0) ||
                 (data_file && strcmp (data_file, input_file) == 0) ||
                 (file && strcmp (file, input_file) == 0) )
            {
                /* We found it, so load the thumbnail.  */
                char *lut_basename = extract_lut_name(polsarpro_aux_info);
                set_input_image_thumbnail (&iter, metadata_file, data_file,
                                           ancillary_file, lut_basename);
                g_free (metadata_file);
		g_free (data_file);
                g_free (input_file);
                g_free (ancillary_file);
                FREE(lut_basename);
                return;
            }

            g_free (input_file);
            g_free (ancillary_file);

            valid = gtk_tree_model_iter_next (GTK_TREE_MODEL (list_store),
                                              &iter);
        }

    /* The data file must have gotten removed from the list before we
    got a chance to draw it's thumbnail.  Oh well.  */

        g_free (metadata_file);
	g_free (data_file);
    //}
}

#endif

static char *build_band_list(const char *file)
{
    // this only applies to ALOS data -- other types we'll just return "-"
    int pre = has_prepension(file);
    if (pre > 0)
    {
        int ii,nBands;
        char *ret;
        char filename[255], dirname[255];

        char **dataName, *baseName;
        baseName = MALLOC(sizeof(char)*255);

        split_dir_and_file(file, dirname, filename);
        char *s = MALLOC(sizeof(char)*(strlen(file)+1));
        sprintf(s, "%s%s", dirname, filename + pre);
        get_ceos_data_name(s, baseName, &dataName, &nBands);

        if (nBands <= 1) {
            // not multiband
            ret = STRDUP("-");
        }
        else {
            // 8 characters per band, plus ", " means 10 characters
            // allocated per band, plus 2 extra just for fun
            ret = MALLOC(sizeof(char)*(nBands*10+2));

            // kludge: assume that band names come between 1st & 2nd dashes (-)
            for (ii=0; ii<nBands; ++ii) {
                char *basename = get_basename(dataName[ii]);

                // point to first -, or the start of the string
                char *p = strchr(basename, '-');
                if (!p) p = basename;

                // point to second -, or the end of the string
                char *q = strchr(p + 1, '-');
                if (!q) q = basename + strlen(basename);

                // if more than 8 characters, just use the first 8.
                if (q-p > 8) q = p + 8;
                *q = '\0';

                if (ii==0)
                    strcpy(ret, p+1);
                else
                    strcat(ret, p+1);

                if (ii < nBands - 1)
                    strcat(ret, ", ");

                FREE(basename);
            }
        }

        free_ceos_names(dataName, NULL);
        FREE(s);
        FREE(baseName);

        return ret;
    }
    else
    {
        return STRDUP("-");
    }
}

gboolean
add_to_files_list(const gchar * data_file)
{
    GtkTreeIter iter;
    gboolean ret = add_to_files_list_iter(data_file, NULL, NULL, NULL, NULL, NULL, NULL, NULL, &iter);
    return ret;
}

static int get_intermediate(const char *str, const char *tag, char **dest)
{
  if (strncmp_case(str, tag, strlen(tag)) == 0) {
    // matched the tag!  skip tag itself
    const char *p = str + strlen(tag);
    // now skip whitespace
    while (isspace(*p)) ++p;
    // now skip a ":" character
    if (*p==':') ++p;
    // now skip more whitespace
    while (isspace(*p)) ++p;
    // the rest is what we were looking for
    *dest = STRDUP(p);
    // strip trailing whitespace
    while (isspace((*dest)[strlen(*dest)-1]))
      (*dest)[strlen(*dest)-1] = '\0';
    return TRUE;
  }
  return FALSE;
}

void
move_to_completed_files_list(GtkTreeIter *iter, GtkTreeIter *completed_iter,
                             const gchar *log_txt,
                             const char *intermediates_file)
{
    // iter: points into "files_list"
    // completed_iter: (returned) points into "completed_files_list"
    gchar *output_file, *output_file_basename;
    gchar *file, *file_basename;
    gchar *ancillary_file;
    gchar *original_metadata_file;
    gchar *polsarpro_aux_info;
    gchar *interferogram_file;
    gchar *coherence_file;
    gchar *slave_metadata_file;
    gchar *baseline_file;

    GtkTreeModel *model = GTK_TREE_MODEL(list_store);
    gtk_tree_model_get(model, iter,
                       COL_INPUT_FILE, &file,
                       COL_INPUT_FILE_SHORT, &file_basename,
                       COL_OUTPUT_FILE, &output_file,
                       COL_OUTPUT_FILE_SHORT, &output_file_basename,
                       COL_ANCILLARY_FILE, &ancillary_file,
                       COL_METADATA_FILE, &original_metadata_file,
                       COL_POLSARPRO_INFO, &polsarpro_aux_info,
		       COL_INTERFEROGRAM, &interferogram_file,
		       COL_COHERENCE, &coherence_file,
		       COL_SLAVE_METADATA, &slave_metadata_file,
		       COL_BASELINE, &baseline_file,
                       -1);

    // pull out the useful intermediates
    char *layover_mask=NULL, *clipped_dem=NULL, *simulated_sar=NULL,
      *tmp_dir=NULL, *faraday=NULL, *hist=NULL, *class_map=NULL,
      *meta_file=NULL, *incid_angles=NULL;

    int i, num_outputs=0, max_outputs=64;
    char **outs = MALLOC(sizeof(char*)*max_outputs);
    for (i=0; i<max_outputs; ++i)
      outs[i] = NULL;

    char line[512];
    FILE *fp = fopen(intermediates_file, "r");
    if (fp) {
      while (fgets(line, 511, fp)) {
        get_intermediate(line, "Layover/Shadow Mask", &layover_mask);
        get_intermediate(line, "Clipped DEM", &clipped_dem);
        get_intermediate(line, "Simulated SAR", &simulated_sar);
        get_intermediate(line, "Temp Dir", &tmp_dir);
        get_intermediate(line, "Faraday", &faraday);
        get_intermediate(line, "Cloude-Pottier Histogram", &hist);
        get_intermediate(line, "Entropy-Alpha Class Map", &class_map);
        get_intermediate(line, "Meta", &meta_file);
        get_intermediate(line, "Incidence Angles", &incid_angles); 
        if (get_intermediate(line, "Output", &outs[num_outputs]))
          if (num_outputs < max_outputs-1)
            ++num_outputs;
      }
      fclose(fp);
    }

    if (!tmp_dir) tmp_dir = STRDUP("");
    if (!layover_mask) layover_mask = STRDUP("");
    if (!clipped_dem) clipped_dem = STRDUP("");
    if (!simulated_sar) simulated_sar = STRDUP("");
    if (!faraday) faraday = STRDUP("");
    if (!hist) hist = STRDUP("");
    if (!class_map) class_map = STRDUP("");
    if (!meta_file) meta_file = STRDUP("");
    if (!incid_angles) incid_angles = STRDUP("");

    //asfPrintStatus("  LO: %s\n  CD: %s\n  SS: %s\n  TD: %s\n  FD: %s\n  CP: %s\n  EA: %s\n  ME: %s\n  IA: %s\n  OP: %S\n",
    //		    layover_mask, clipped_dem, simulated_sar, tmp_dir, faraday, hist, class_map, meta_file, incid_angles, 
    //		    outs[0] );

    // now add to the completed files list!  Use the first listed file
    // as the output filename, since that is the one that was thumbnailed
    // Exception: PolSARPro.  Here we use the second one (if we have it)
    // since the first is the amplitude, and likely the user is interested
    // in the second one (the PolSARPro one).
    int output_idx = 0;
    if (polsarpro_aux_info && strlen(polsarpro_aux_info)>0 && num_outputs>1)
      output_idx = 1;

    gtk_list_store_append(completed_list_store, completed_iter);
    gtk_list_store_set(completed_list_store, completed_iter,
                       COMP_COL_INPUT_FILE, file,
                       COMP_COL_INPUT_FILE_SHORT, file_basename,
                       COMP_COL_ANCILLARY_FILE, ancillary_file,
                       COMP_COL_ORIGINAL_METADATA_FILE, original_metadata_file,
                       COMP_COL_OUTPUT_FILE, outs[output_idx],
                       COMP_COL_OUTPUT_FILE_SHORT, output_file_basename,
                       COMP_COL_STATUS, "Done",
                       COMP_COL_LOG, log_txt,
                       COMP_COL_TMP_DIR, tmp_dir,
                       COMP_COL_LAYOVER_SHADOW_MASK_FILE, layover_mask,
                       COMP_COL_CLIPPED_DEM_FILE, clipped_dem,
                       COMP_COL_SIMULATED_SAR_FILE, simulated_sar,
                       COMP_COL_FARADAY_FILE, faraday,
                       COMP_COL_HIST_FILE, hist,
                       COMP_COL_CLASS_MAP_FILE, class_map,
                       COMP_COL_METADATA_FILE, meta_file,
                       COMP_COL_POLSARPRO_INFO, polsarpro_aux_info,
		       COMP_COL_INTERFEROGRAM, interferogram_file,
		       COMP_COL_COHERENCE, coherence_file,
		       COMP_COL_SLAVE_METADATA, slave_metadata_file,
		       COMP_COL_BASELINE, baseline_file,
           COMP_COL_INCID_ANGLES_FILE, incid_angles,
                       -1);

    // remove from the input list
    gtk_list_store_remove(GTK_LIST_STORE(model), iter);

    free(layover_mask);
    free(clipped_dem);
    free(simulated_sar);
    free(tmp_dir);
    free(faraday);
    free(hist);
    free(class_map);
    free(meta_file);

    for (i=0; i<num_outputs; ++i)
      FREE(outs[i]);
    FREE(outs);

    g_free(file);
    g_free(output_file);
    g_free(file_basename);
    g_free(output_file_basename);
    g_free(ancillary_file);
    g_free(original_metadata_file);
    g_free(polsarpro_aux_info);

    // hide/show the ancillary files column and update the input formats
    // list, now that one of the files has been removed
    refresh_file_names();
    input_data_formats_changed();
}

void
move_from_completed_files_list(GtkTreeIter *iter)
{
    gchar *input_file;
    gchar *ancillary_file;
    gchar *meta_file;
    gchar *tmp_dir;
    gchar *polsarpro_aux_info;
    gchar *interferogram_file;
    gchar *coherence_file;
    gchar *slave_metadata_file;
    gchar *baseline_file;

    GtkTreeModel *model = GTK_TREE_MODEL(completed_list_store);
    gtk_tree_model_get(model, iter,
                       COMP_COL_INPUT_FILE, &input_file,
                       COMP_COL_ANCILLARY_FILE, &ancillary_file,
                       COMP_COL_ORIGINAL_METADATA_FILE, &meta_file,
                       COMP_COL_TMP_DIR, &tmp_dir,
                       COMP_COL_POLSARPRO_INFO, &polsarpro_aux_info,
		       COMP_COL_INTERFEROGRAM, &interferogram_file,
		       COMP_COL_COHERENCE, &coherence_file,
		       COMP_COL_SLAVE_METADATA, &slave_metadata_file,
		       COMP_COL_BASELINE, &baseline_file,
                       -1);

    if (get_checked("rb_keep_temp") && tmp_dir && strlen(tmp_dir) > 0) {
      asfPrintStatus("Removing: %s\n", tmp_dir);
      remove_dir(tmp_dir);
    }

    move_to_files_list(input_file, ancillary_file, meta_file,
                       polsarpro_aux_info, interferogram_file, coherence_file,
		       slave_metadata_file, baseline_file);

    gtk_list_store_remove(GTK_LIST_STORE(model), iter);

    g_free(input_file);
    g_free(ancillary_file);
    g_free(meta_file);
    g_free(tmp_dir);
    g_free(polsarpro_aux_info);
    /*
    g_free(interferogram_file);
    g_free(coherence_file);
    g_free(slave_metadata_file);
    g_free(baseline_file);
    */
}

// The thumbnailing works like this: When a user adds a file, or a bunch of
// files, the file names are added immediately to the list, and each data
// file name is added to this global thumbnailing queue.  (It isn't really
// a queue, it is just an array of char* pointers.)  After all the files have
// been added to the list, and queue_thumbnail has been called for each one,
// we call show_queued_thumbnails(), which runs through the list of saved
// names, and populates the thumbnails for each.  So it appears as though
// we are still threading but everything occurs sequentially.  The only
// tricky thing is that show_queued_thumbnails periodically processes the
// gtk events, to keep the app from seeming unresponsive.  This isn't a
// problem, except that one possible event is removing files that were
// added, another is adding even more files, which will queue up more
// thumbnails, which means show_queued_thumbnails can recurse.  This isn't
// really a problem if we keep in mind that the gtk_main_iteration() call
// in show_queued_thumbnails() can result in the thumb_files[] array
// changing on us (sort of as if we were doing threading).

#define QUEUE_SIZE 255
static char *thumb_files[QUEUE_SIZE];
static void queue_thumbnail(const gchar * data_file)
{
    int i;
    for (i = 0; i < QUEUE_SIZE; ++i) {
        if (!thumb_files[i]) {
            thumb_files[i] = g_strdup(data_file);
            break;
        }
    }
}

// This is just an externally available wrapper for
// queue_thumbnail() (necessary in file_selection.c)
void add_thumbnail(const gchar *data_file)
{
  queue_thumbnail(data_file);
}

void
show_queued_thumbnails()
{
    int i;
    for (i = 0; i < QUEUE_SIZE; ++i) {
      if (thumb_files[i]) {
            // do a gtk main loop iteration
            while (gtk_events_pending())
                gtk_main_iteration();

            // must check files[i]!=NULL again, since gtk_main_iteration could
            // have processed a "Browse..." event and thus already processed
            // this item.  The alternative would be to move the above while()
            // loop below the statements below, however that makes the app feel
            // a little less responsive.
            if (thumb_files[i]) {
              do_thumbnail(thumb_files[i]);

              g_free(thumb_files[i]);
              thumb_files[i] = NULL;
            }
        }
    }
}

gboolean move_to_files_list(const gchar *data_file,
                            const gchar *ancillary_file,
                            const gchar *meta_file,
                            const gchar *polsarpro_aux_info,
			    const gchar *interferogram_file,
			    const gchar *coherence_file,
			    const gchar *slave_metadata_file,
			    const gchar *baseline_file)
{
  GtkTreeIter iter;
  gboolean ret;
  
  if (data_file      && strlen(data_file) &&
      meta_file      && strlen(meta_file) &&
      ((interferogram_file && strlen(interferogram_file)) ||
       (coherence_file && strlen(coherence_file)) ||
       (slave_metadata_file && strlen(slave_metadata_file) &&
	baseline_file && strlen(baseline_file))))
  {
    ret = add_to_files_list_iter(data_file, ancillary_file,
                                 meta_file, polsarpro_aux_info,
				 interferogram_file, coherence_file,
				 slave_metadata_file, baseline_file,
                                 &iter);
  }
  else if (ancillary_file && strlen(ancillary_file) &&
      data_file      && strlen(data_file) &&
      meta_file      && strlen(meta_file))
  {
    ret = add_to_files_list_iter(data_file, ancillary_file,
                                 meta_file, polsarpro_aux_info,
				 NULL, NULL, NULL, NULL, &iter);
  }
  else if (meta_file && strlen(meta_file) &&
           data_file && strlen(data_file))
  {
    ret = add_to_files_list_iter(data_file, NULL, meta_file, 
                                 polsarpro_aux_info, NULL, NULL, NULL, NULL,
				 &iter);
  }
  else if (ancillary_file && strlen(ancillary_file) &&
           data_file      && strlen(data_file))
  {
    ret = add_to_files_list_iter(data_file, ancillary_file, NULL,
                                 polsarpro_aux_info, NULL, NULL, NULL, NULL,
				 &iter);
  }
  else if (data_file && strlen(data_file)) {
    ret = add_to_files_list_iter(data_file, NULL, NULL,
                                 polsarpro_aux_info, NULL, NULL, NULL, NULL,
				 &iter);
  }
  else {
    ret = FALSE;
  }

  return ret;
}

gboolean
add_to_files_list_iter(const gchar *input_file_in,
                       const gchar *ancillary_file_in,
                       const gchar *meta_file_in,
                       const gchar *polsarpro_aux_info,
		       const gchar *interferogram_file,
		       const gchar *coherence_file,
		       const gchar *slave_metadata_file,
		       const gchar *baseline_file,
                       GtkTreeIter *iter_p)
{
    char *input_file = file_is_valid(input_file_in);
    if (!input_file && meta_file_in && strlen(meta_file_in) > 0) {
      // gamma data -- file_is_valid() returns false for gamma...
      input_file = STRDUP(input_file_in);
    }
    char *ancillary_file_valid = NULL;
    int valid = input_file != NULL;

    // NOTE: When a file is added to the input list for the first time,
    // the ancillary_file_in will be NULL or zero length.  When a file is
    // moved from the completed files back to the list of input files, then
    // ancillary_file will be valid and will have a full
    // path/filename in it (see move_to_files_list() and add_to_files_list() )
    if (valid &&
        ancillary_file_in != NULL &&
        strlen(ancillary_file_in) > 0)
    {
      ancillary_file_valid = file_is_valid(ancillary_file_in);
    }

    if (valid)
    {
        /* If this file is already in the list, ignore it */
        GtkTreeIter iter;
        int found = FALSE;
        gboolean more_items =
          gtk_tree_model_get_iter_first(GTK_TREE_MODEL(list_store), &iter);
        while (more_items) {
          gchar *input_file_in_list;
	  gchar *igram_file_in_list;
	  gchar *coh_file_in_list;
          gtk_tree_model_get(GTK_TREE_MODEL(list_store), &iter,
                             COL_INPUT_FILE, &input_file_in_list, -1);
          gtk_tree_model_get(GTK_TREE_MODEL(list_store), &iter,
                             COL_INTERFEROGRAM, &igram_file_in_list, -1);
          gtk_tree_model_get(GTK_TREE_MODEL(list_store), &iter,
                             COL_COHERENCE, &coh_file_in_list, -1);
	  if (interferogram_file && 
	      strcmp(interferogram_file, igram_file_in_list) == 0)
	    found = TRUE;
	  else if (coherence_file &&
		   strcmp(coherence_file, coh_file_in_list) == 0)
	    found = TRUE;
          else if (!interferogram_file && !coherence_file &&
		   strcmp(input_file, input_file_in_list) == 0)
            found = TRUE;
          g_free(input_file_in_list);
          if (found)
            break;
          more_items = gtk_tree_model_iter_next (GTK_TREE_MODEL (list_store),
                                                 &iter);
        }

        if (found) {
          asfPrintStatus("File '%s' is already in the list, skipping.\n",
                         input_file_in);
        }
        else {
          /* not already in list -- add it */

          // Build list of bands
          char *bands = build_band_list(input_file);

          // Populate the input file fields
          // (full path version and filename-only version)
          gchar *basename = g_path_get_basename(input_file);
          gchar *ancillary_basename, *ancillary_fullname;
          if (ancillary_file_valid != NULL) {
            ancillary_basename = g_path_get_basename(ancillary_file_in);
            ancillary_fullname = g_strdup(ancillary_file_in);
          }
          else {
            ancillary_basename = g_strdup("");
            ancillary_fullname = g_strdup("");
          }
          gchar *meta_basename, *meta_fullname;
          if (meta_file_in != NULL) {
            meta_basename = g_path_get_basename(meta_file_in);
            meta_fullname = g_strdup(meta_file_in);
          }
          else {
            meta_basename = g_strdup("");
            meta_fullname = g_strdup("");
          }
          char *aux_files, *aux_files_short;
          if (meta_file_in != NULL && ancillary_file_valid != NULL) {
            int len = strlen(meta_fullname)+strlen(ancillary_fullname)+32;
            aux_files = MALLOC(sizeof(char)*len);
            sprintf(aux_files, "CEOS: %s\nMetadata: %s",
                    ancillary_fullname, meta_fullname);
            len = strlen(meta_basename)+strlen(ancillary_basename)+32;
            aux_files_short = MALLOC(sizeof(char)*len);
            sprintf(aux_files_short, "CEOS: %s\nMetadata: %s",
                    ancillary_basename, meta_basename);
          }
          else if (meta_file_in != NULL) {
            aux_files = STRDUP(meta_fullname);
            aux_files_short = STRDUP(meta_basename);
          }
          else if (ancillary_file_valid != NULL) {
            aux_files = STRDUP(ancillary_fullname);
            aux_files_short = STRDUP(ancillary_basename);
          }
          else {
            aux_files = STRDUP("");
            aux_files_short = STRDUP("");
          }

          gchar *polsarpro_display;
          if (polsarpro_aux_info && strlen(polsarpro_aux_info)>0) {
            char *p = strchr(polsarpro_aux_info, ';');
            if (p) {
              polsarpro_display =
                g_malloc(sizeof(gchar)*strlen(polsarpro_aux_info)+64);
              int is_classification = polsarpro_aux_info[0]=='0';
              int is_colormapped = strcmp_case(p+1, "none")!=0;
              const char *c = is_classification ? " (Segmentation)" : "";
              if (!is_colormapped && !is_classification) {
                strcpy(polsarpro_display, "-");
              }
              else if (!is_colormapped) {
                sprintf(polsarpro_display, "Greyscale%s", c);
              }
              else {
                sprintf(polsarpro_display, "%s%s", p+1, c);
              }
            }
            else {
              printf("Invalid PolSARPro aux info found.\n");
              polsarpro_display = g_strdup("");
            }
          }
          else {
            polsarpro_display = g_strdup("");
          }

          // ready to add to the list
          gtk_list_store_append(list_store, iter_p);
          gtk_list_store_set(list_store, iter_p,
                             COL_INPUT_FILE, input_file,
                             COL_INPUT_FILE_SHORT, basename,
                             COL_ANCILLARY_FILE, ancillary_fullname,
                             COL_METADATA_FILE, meta_fullname,
                             COL_ALL_AUX_FILES, aux_files,
                             COL_ALL_AUX_FILES_SHORT, aux_files_short,
                             COL_BAND_LIST, bands,
                             COL_POLSARPRO_INFO, polsarpro_aux_info,
                             COL_POLSARPRO_DISPLAY, polsarpro_display,
			     COL_INTERFEROGRAM, interferogram_file,
			     COL_COHERENCE, coherence_file,
			     COL_SLAVE_METADATA, slave_metadata_file,
			     COL_BASELINE, baseline_file,
                             COL_STATUS, "-",
                             COL_LOG, "Has not been processed yet.",
                             -1);

          g_free(basename);
          g_free(ancillary_basename);
          g_free(meta_basename);
          g_free(ancillary_fullname);
          g_free(meta_fullname);
          g_free(polsarpro_display);
	  /*
	  g_free(interferogram_file);
	  g_free(coherence_file);
	  g_free(slave_metadata_file);
	  g_free(baseline_file);
	  */
	  
          free(aux_files);
          free(aux_files_short);

          // Determine output file name
          gchar * out_name_full;
	  if (interferogram_file && strlen(interferogram_file) > 0)
	    out_name_full = 
	      determine_default_output_file_name(interferogram_file);
	  else if (coherence_file && strlen(coherence_file) > 0)
	    out_name_full = 
	      determine_default_output_file_name(coherence_file);
	  else
	    out_name_full = determine_default_output_file_name(input_file);
          set_output_name(iter_p, out_name_full);
          g_free(out_name_full);
          FREE(bands);

          // Add the file to the thumbnail queue
          queue_thumbnail(input_file);

          // Update the visible/invisible widgets in the input section,
          // to reflect what kind of data we have
          input_data_formats_changed();

          // Hide/show the ancillary files column
          refresh_file_names();

          /* Select the file automatically if this is the first
             file that was added (this makes the toolbar buttons
             immediately useful)                                 */
          if (1 == gtk_tree_model_iter_n_children(GTK_TREE_MODEL(list_store),
                                                  NULL))
          {
            GtkWidget *files_list = get_widget_checked("files_list");
            GtkTreeSelection *selection =
              gtk_tree_view_get_selection(GTK_TREE_VIEW(files_list));
            gtk_tree_selection_select_all(selection);
          }
        }

        free(input_file);
    }

    return valid;
}

void
update_all_extensions()
{
    Settings * user_settings;
    const gchar * ext;
    gboolean ok;
    GtkTreeIter iter;

    if (list_store)
    {
        user_settings = settings_get_from_gui();
        ext = settings_get_output_format_extension(user_settings);

        ok = gtk_tree_model_get_iter_first(GTK_TREE_MODEL(list_store), &iter);
        while (ok)
        {
            gchar * current_output_name;
            gchar * polsarpro_aux_info;
            gchar * new_output_name;
            gchar * basename;
            gchar * p;

            gtk_tree_model_get(GTK_TREE_MODEL(list_store), &iter,
			       COL_OUTPUT_FILE, &current_output_name, 
			       COL_POLSARPRO_INFO, &polsarpro_aux_info, -1);
	    int image_data_type = extract_image_data_type(polsarpro_aux_info);

            basename = g_strdup(current_output_name);
            p = strrchr(basename, '.');
            if (p)
                *p = '\0';

            new_output_name =
                (gchar *) g_malloc(sizeof(gchar) * (strlen(basename) +
                strlen(ext) + 2));

	    if (image_data_type == SELECT_POLARIMETRIC_MATRIX ||
		image_data_type == SELECT_POLARIMETRIC_DECOMPOSITION)
	      g_sprintf(new_output_name, "%s", basename);
	    else
	      g_sprintf(new_output_name, "%s.%s", basename, ext);

            set_output_name(&iter, new_output_name);

            g_free(basename);
            g_free(new_output_name);
            g_free(current_output_name);
	    g_free(polsarpro_aux_info);

            ok = gtk_tree_model_iter_next(GTK_TREE_MODEL(list_store), &iter);
        }

        settings_delete(user_settings);
    }
}

void
edited_handler(GtkCellRendererText *ce, gchar *arg1, gchar *arg2,
               gpointer user_data)
{
    /* arg1 indicates which row -- should assert() that it matches
    the selected row, since we're asssuming that */
    do_rename_selected(arg2);
}

/* Original, but unused render_status */
/*void render_status(GtkTreeViewColumn *tree_column,
                   GtkCellRenderer *cell,
                   GtkTreeModel *tree_model,
                   GtkTreeIter *iter,
                   gpointer data)
{
    gchar *status;
    gboolean done;
    gboolean settings_are_stale = FALSE;

    gtk_tree_model_get (tree_model, iter, COL_STATUS, &status, -1);
    done = strcmp(status, "Done") == 0;

    if (done && settings_on_execute)
    {
        Settings * user_settings =
            settings_get_from_gui();

        settings_are_stale =
            !settings_equal(user_settings, settings_on_execute);

        settings_delete(user_settings);
    }

    if (done && settings_are_stale)
    {
        GdkColor c;

        c.red = c.green = c.blue = 32768;

        g_object_set( G_OBJECT (cell), "foreground-gdk", &c, NULL);
    }
    else
    {
        g_object_set( G_OBJECT (cell), "foreground-gdk", NULL, NULL);
    }

    g_object_set (G_OBJECT (cell), "text", status, NULL);
    g_free(status);
}
*/
/* New render_status - turns status red if an error statement has
   been issued (status begins with "Error:") or if status begins
   with "Select:" .. to-do for the user */
void render_status(GtkTreeViewColumn *tree_column,
                   GtkCellRenderer *cell,
                   GtkTreeModel *tree_model,
                   GtkTreeIter *iter,
                   gpointer data)
{
    gchar *status;
    gboolean done;
    gboolean processing;
    gboolean error_occurred;
    gboolean user_todo_exists;

    gtk_tree_model_get (tree_model, iter, COL_STATUS, &status, -1);
    done             = strcmp ("Done",                 status)     == 0;
    processing       = strcmp ("Processing...",        status)     == 0;
    error_occurred   = strncmp("Error:",              status, 6)  == 0;
    user_todo_exists = strncmp("Select:",             status, 7)  == 0 ||
                       strncmp("Add Ancillary File:", status, 19) == 0;

    if (!done && !processing &&
        (error_occurred   ||
         user_todo_exists ))
    {
        // Condition RED...
        GdkColor c;

        c.red = 65535;
        c.green = c.blue = 0;

        g_object_set( G_OBJECT (cell), "foreground-gdk", &c, NULL);
    }
    else
    {
        // Condition SO WHAT...
        g_object_set( G_OBJECT (cell), "foreground-gdk", NULL, NULL);
    }

    g_object_set (G_OBJECT (cell), "text", status, NULL);
    g_free(status);
}

void render_output_name(GtkTreeViewColumn *tree_column,
                              GtkCellRenderer *cell,
                              GtkTreeModel *tree_model,
                              GtkTreeIter *iter,
                              gpointer data)
{
  gchar *output_file;
  gchar *long_output_file;
  gchar *status;
  gboolean done;
  gboolean processing;

  gtk_tree_model_get (tree_model, iter,
                      COL_OUTPUT_FILE, &long_output_file,
                      COL_OUTPUT_FILE_SHORT, &output_file,
                      COL_STATUS, &status,
                      -1);

  /* Do not mark the file in red if the item has been marked "Done"
     However, if the user has changed the settings, the "Done"
     marks are stale... so in that case do not look at "Done" */

  done = strcmp("Done", status) == 0;
  processing = strcmp("Processing...", status) == 0;

  if (done && settings_on_execute)
  {
    Settings * user_settings =
        settings_get_from_gui();

    if (!settings_equal(user_settings, settings_on_execute))
      done = FALSE;

    settings_delete(user_settings);
  }

  if (!processing && !done &&
       g_file_test(long_output_file, G_FILE_TEST_EXISTS))
  {
    GdkColor c;

    c.red = 65535;
    c.green = c.blue = 0;

    g_object_set( G_OBJECT (cell), "foreground-gdk", &c, NULL);
  }
  else
  {
    g_object_set( G_OBJECT (cell), "foreground-gdk", NULL, NULL);
  }

  g_object_set (G_OBJECT (cell), "text",
                (show_full_paths) ? long_output_file : output_file,
                NULL);

  g_free(output_file);
  g_free(long_output_file);
  g_free(status);
}

void
populate_files_list(int argc, char *argv[])
{
    int i;
    for (i = 1; i < argc; ++i)
    {
        char *ext = findExt(argv[i]);
        if (ext && (strcmp(ext, ".cfg") == 0 || strcmp(ext, ".config") == 0))
            apply_settings_from_config_file(argv[i]);
        else
            add_to_files_list(argv[i]);
    }

    show_queued_thumbnails();
}

void
setup_files_list()
{
    GtkTreeViewColumn *col;
    GtkCellRenderer *renderer;

    list_store = gtk_list_store_new(18,
                                    G_TYPE_STRING,    // Input file - Full path (usually hidden)
                                    G_TYPE_STRING,    // Input file - No path
                                    G_TYPE_STRING,    // Ancillary file - Full path (usually hidden)
                                    G_TYPE_STRING,    // Meta file - Full path (usually hidden)
                                    G_TYPE_STRING,    // All aux files - Full path (usually hidden)
                                    G_TYPE_STRING,    // All aux files - No path (usually hidden)
                                    GDK_TYPE_PIXBUF,  // Input thumbnail
                                    G_TYPE_STRING,    // Bands
                                    G_TYPE_STRING,    // Output file - Full path (usually hidden)
                                    G_TYPE_STRING,    // Output file - No path
                                    G_TYPE_STRING,    // Status
                                    G_TYPE_STRING,    // Log (hidden)
                                    G_TYPE_STRING,    // PolSARPRo aux info (hidden)
                                    G_TYPE_STRING,    // PolSARPRo aux display
				    G_TYPE_STRING,    // Interferogram (hidden)
				    G_TYPE_STRING,    // Coherence image (hidden)
				    G_TYPE_STRING,    // Slave metadata (hidden)
				    G_TYPE_STRING);   // Baseline (hidden)

    COL_INPUT_FILE = 0;
    COL_INPUT_FILE_SHORT = 1;
    COL_ANCILLARY_FILE = 2;
    COL_METADATA_FILE = 3;
    COL_ALL_AUX_FILES = 4;
    COL_ALL_AUX_FILES_SHORT = 5;
    COL_INPUT_THUMBNAIL = 6;
    COL_BAND_LIST = 7;
    COL_OUTPUT_FILE = 8;
    COL_OUTPUT_FILE_SHORT = 9;
    COL_STATUS = 10;
    COL_LOG = 11;
    COL_POLSARPRO_INFO = 12;
    COL_POLSARPRO_DISPLAY = 13;
    COL_INTERFEROGRAM = 14;
    COL_COHERENCE = 15;
    COL_SLAVE_METADATA = 16;
    COL_BASELINE = 17;

    completed_list_store = gtk_list_store_new(24,
                                              G_TYPE_STRING,    // Data file-Full path (usually hid.)
                                              G_TYPE_STRING,    // Data file - No path
                                              G_TYPE_STRING,    // Ancillary file-Full path (hid.)
                                              G_TYPE_STRING,    // Output file-Full path (usually hid.)
                                              G_TYPE_STRING,    // Output file - No path
                                              GDK_TYPE_PIXBUF,  // Output thumbnail
                                              GDK_TYPE_PIXBUF,  // Big output thumbnail (show on hover)
                                              G_TYPE_STRING,    // Status
                                              G_TYPE_STRING,    // Log (hidden)
                                              G_TYPE_STRING,    // Temp folder (hidden)
                                              G_TYPE_STRING,    // Layover shadow mask file (hidden)
                                              G_TYPE_STRING,    // Clipped DEM file (hidden)
                                              G_TYPE_STRING,    // Simulated SAR file (hidden)
                                              G_TYPE_STRING,    // Faraday rotations file (hidden)
                                              G_TYPE_STRING,    // History file (hidden)
                                              G_TYPE_STRING,    // Class map file (hidden)
                                              G_TYPE_STRING,    // Original metadata file (hidden)
                                              G_TYPE_STRING,    // Metadata file (hidden)
                                              G_TYPE_STRING,    // PolSARPro aux info (hidden)
					      G_TYPE_STRING,    // Interferogram (hidden)
					      G_TYPE_STRING,    // Coherence (hidden)
					      G_TYPE_STRING,    // Slave metadata (hidden)
					      G_TYPE_STRING,    // Baseline (hidden)
                G_TYPE_STRING);   // Incidence Angles File (hidden)

    COMP_COL_INPUT_FILE = 0;
    COMP_COL_INPUT_FILE_SHORT = 1;
    COMP_COL_ANCILLARY_FILE = 2;
    COMP_COL_OUTPUT_FILE = 3;
    COMP_COL_OUTPUT_FILE_SHORT = 4;
    COMP_COL_OUTPUT_THUMBNAIL = 5;
    COMP_COL_OUTPUT_THUMBNAIL_BIG = 6;
    COMP_COL_STATUS = 7;
    COMP_COL_LOG = 8;
    COMP_COL_TMP_DIR = 9;
    COMP_COL_LAYOVER_SHADOW_MASK_FILE = 10;
    COMP_COL_CLIPPED_DEM_FILE = 11;
    COMP_COL_SIMULATED_SAR_FILE = 12;
    COMP_COL_FARADAY_FILE = 13;
    COMP_COL_HIST_FILE = 14;
    COMP_COL_CLASS_MAP_FILE = 15;
    COMP_COL_ORIGINAL_METADATA_FILE = 16;
    COMP_COL_METADATA_FILE = 17;
    COMP_COL_POLSARPRO_INFO = 18;
    COMP_COL_INTERFEROGRAM = 19;
    COMP_COL_COHERENCE = 20;
    COMP_COL_SLAVE_METADATA = 21;
    COMP_COL_BASELINE = 22;
    COMP_COL_INCID_ANGLES_FILE = 23;

/*** First, the "pending" files list ****/
    GtkWidget *files_list = get_widget_checked("files_list");

    /* First Column: Input File Name (full path) */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Input File");
    gtk_tree_view_column_set_visible(col, (show_full_paths) ? TRUE : FALSE);
    gtk_tree_view_column_set_resizable(col, TRUE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(files_list), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    g_object_set(renderer, "text", "?", NULL);
    gtk_tree_view_column_add_attribute(col, renderer, "text", COL_INPUT_FILE);

    /* Next Column: Input File Name, but without full path */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Input File");
    gtk_tree_view_column_set_visible(col, (show_full_paths) ? FALSE : TRUE);
    gtk_tree_view_column_set_resizable(col, TRUE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(files_list), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    g_object_set(renderer, "text", "?", NULL);
    gtk_tree_view_column_add_attribute(col, renderer, "text",
                                       COL_INPUT_FILE_SHORT);

    /* First Column: Ancillary File File Name (full path) */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Ancillary File");
    gtk_tree_view_column_set_visible(col, FALSE);
    gtk_tree_view_column_set_resizable(col, TRUE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(files_list), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    g_object_set(renderer, "text", "?", NULL);
    gtk_tree_view_column_add_attribute(col, renderer, "text",
                                       COL_ANCILLARY_FILE);

    /* First Column: Meta File File Name (full path) */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Meta File");
    gtk_tree_view_column_set_visible(col, FALSE);
    gtk_tree_view_column_set_resizable(col, TRUE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(files_list), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    g_object_set(renderer, "text", "?", NULL);
    gtk_tree_view_column_add_attribute(col, renderer, "text",
                                       COL_METADATA_FILE);

    /* Next Column: combined list of all ancillary & meta files, full path */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Ancillary Files");
    gtk_tree_view_column_set_visible(col, FALSE);
    gtk_tree_view_column_set_resizable(col, TRUE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(files_list), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    g_object_set(renderer, "text", "?", NULL);
    gtk_tree_view_column_add_attribute(col, renderer, "text",
                                       COL_ALL_AUX_FILES);

    /* Next Column: combined list of all ancillary & meta files, basename */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Ancillary Files");
    gtk_tree_view_column_set_visible(col, FALSE);
    gtk_tree_view_column_set_resizable(col, TRUE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(files_list), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    g_object_set(renderer, "text", "?", NULL);
    gtk_tree_view_column_add_attribute(col, renderer, "text",
                                       COL_ALL_AUX_FILES_SHORT);

    /* Next Column: thumbnail of input image.  */
    col = gtk_tree_view_column_new ();
    gtk_tree_view_column_set_title (col, "");
    gtk_tree_view_column_set_resizable (col, FALSE);
    gtk_tree_view_append_column (GTK_TREE_VIEW (files_list), col);
    renderer = gtk_cell_renderer_pixbuf_new ();
    gtk_tree_view_column_pack_start (col, renderer, FALSE);
    gtk_tree_view_column_add_attribute (col, renderer, "pixbuf",
                                        COL_INPUT_THUMBNAIL);

    g_signal_connect (files_list, "motion-notify-event",
                      G_CALLBACK (files_list_motion_notify_event_handler),
                      NULL);

    g_signal_connect (files_list, "leave-notify-event",
                      G_CALLBACK (files_list_leave_notify_event_handler),
                      files_list);

    g_signal_connect (files_list, "scroll-event",
                      G_CALLBACK (files_list_scroll_event_handler), NULL);

    /* Next Column: Band List */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Bands");
    gtk_tree_view_column_set_resizable(col, TRUE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(files_list), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    gtk_tree_view_column_add_attribute(col, renderer, "text", COL_BAND_LIST);

    /* Next Column: Output File Name (full path) */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Output File");
    gtk_tree_view_column_set_visible(col, (show_full_paths) ? TRUE : FALSE);
    gtk_tree_view_column_set_resizable(col, TRUE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(files_list), col);
    renderer = gtk_cell_renderer_text_new();

    /* allow editing the output filename right in the grid (full length version) */
    GValue val1={0,};
    g_value_init(&val1, G_TYPE_BOOLEAN);
    g_value_set_boolean(&val1, TRUE);
    g_object_set_property(G_OBJECT(renderer), "editable", &val1);

    /* connect "editing-done" signal */
    g_signal_connect(G_OBJECT(renderer), "edited",
        G_CALLBACK(edited_handler), NULL);

    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    gtk_tree_view_column_add_attribute(col, renderer, "text", COL_OUTPUT_FILE);

    /* add our custom renderer (turns existing files red) */
    gtk_tree_view_column_set_cell_data_func(col, renderer,
                                            render_output_name, NULL, NULL);

    /* Next Column: Output File Name, but without full path */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Output File");
    gtk_tree_view_column_set_visible(col, (show_full_paths) ? FALSE : TRUE);
    gtk_tree_view_column_set_resizable(col, TRUE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(files_list), col);
    renderer = gtk_cell_renderer_text_new();

    /* allow editing the output filename right in the grid (shortened version) */
    GValue val2={0,};
    g_value_init(&val2, G_TYPE_BOOLEAN);
    g_value_set_boolean(&val2, TRUE);
    g_object_set_property(G_OBJECT(renderer), "editable", &val2);

    /* connect "editing-done" signal */
    g_signal_connect(G_OBJECT(renderer), "edited",
                     G_CALLBACK(edited_handler), NULL);

    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    gtk_tree_view_column_add_attribute(col, renderer, "text", COL_OUTPUT_FILE_SHORT);

    /* add our custom renderer (turns existing files red) */
    gtk_tree_view_column_set_cell_data_func(col, renderer,
                                            render_output_name, NULL, NULL);

    /* Next Column: Current Status */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Status");
    gtk_tree_view_column_set_resizable(col, TRUE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(files_list), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    gtk_tree_view_column_add_attribute(col, renderer, "text", COL_STATUS);

    /* add our custom renderer (turns errors and user todo's red) */
    gtk_tree_view_column_set_cell_data_func(col, renderer,
                                            render_status, NULL, NULL);


    /* Next Column: Log Info (hidden) */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Log Info");
    gtk_tree_view_column_set_visible(col, FALSE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(files_list), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    gtk_tree_view_column_add_attribute(col, renderer, "text", COL_LOG);

    /* Next Column: PolSARPro aux information (data, always hidden) */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "");
    gtk_tree_view_column_set_resizable(col, FALSE);
    gtk_tree_view_column_set_visible(col, FALSE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(files_list), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    gtk_tree_view_column_add_attribute(col, renderer, "text",
                                       COL_POLSARPRO_INFO);

    /* Next Column: PolSARPro aux information (user-friendly display info,
                    shown when we have polsarpro data loaded in) */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "PolSARPro Settings");
    gtk_tree_view_column_set_resizable(col, FALSE);
    gtk_tree_view_column_set_visible(col, FALSE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(files_list), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    gtk_tree_view_column_add_attribute(col, renderer, "text",
                                       COL_POLSARPRO_DISPLAY);

    /* Next Column: Interferogram (always hidden) */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Interferogram");
    gtk_tree_view_column_set_resizable(col, FALSE);
    gtk_tree_view_column_set_visible(col, FALSE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(files_list), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    gtk_tree_view_column_add_attribute(col, renderer, "text",
                                       COL_INTERFEROGRAM);

    /* Next Column: Coherence (always hidden) */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Coherence");
    gtk_tree_view_column_set_resizable(col, FALSE);
    gtk_tree_view_column_set_visible(col, FALSE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(files_list), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    gtk_tree_view_column_add_attribute(col, renderer, "text",
                                       COL_COHERENCE);

    /* Next Column: Slave metadata (always hidden) */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Slave metadata");
    gtk_tree_view_column_set_resizable(col, FALSE);
    gtk_tree_view_column_set_visible(col, FALSE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(files_list), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    gtk_tree_view_column_add_attribute(col, renderer, "text",
                                       COL_SLAVE_METADATA);

    /* Next Column: Baseline (always hidden) */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Baseline");
    gtk_tree_view_column_set_resizable(col, FALSE);
    gtk_tree_view_column_set_visible(col, FALSE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(files_list), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    gtk_tree_view_column_add_attribute(col, renderer, "text",
                                       COL_BASELINE);

    gtk_tree_view_set_model(GTK_TREE_VIEW(files_list),
        GTK_TREE_MODEL(list_store));

    g_object_unref(list_store);

    gtk_tree_selection_set_mode(
        gtk_tree_view_get_selection(GTK_TREE_VIEW(files_list)),
        GTK_SELECTION_MULTIPLE);

/*** Second, the "completed" files list ****/
    GtkWidget *completed_files_list =
        get_widget_checked("completed_files_list");

    /* First Column: Input File Name (with full path) */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Data File");
    gtk_tree_view_column_set_visible(col, (show_full_paths) ? TRUE : FALSE);
    gtk_tree_view_column_set_resizable(col, TRUE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(completed_files_list), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    g_object_set(renderer, "text", "?", NULL);
    gtk_tree_view_column_add_attribute(col, renderer, "text",
                                       COMP_COL_INPUT_FILE);

    /* Next Column: Input File Name (no path) */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Data File");
    gtk_tree_view_column_set_visible(col, (show_full_paths) ? FALSE : TRUE);
    gtk_tree_view_column_set_resizable(col, TRUE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(completed_files_list), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    g_object_set(renderer, "text", "?", NULL);
    gtk_tree_view_column_add_attribute(col, renderer, "text",
                                       COMP_COL_INPUT_FILE_SHORT);

    /* Next Column: Ancillary File Name (with full path) (hidden) */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Ancillary File");
    gtk_tree_view_column_set_visible(col, FALSE);
    gtk_tree_view_column_set_resizable(col, TRUE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(completed_files_list), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    g_object_set(renderer, "text", "?", NULL);
    gtk_tree_view_column_add_attribute(col, renderer, "text",
                                       COMP_COL_ANCILLARY_FILE);

    /* Next Column: Output File Name (full path) */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Output File");
    gtk_tree_view_column_set_visible(col, (show_full_paths) ? TRUE : FALSE);
    gtk_tree_view_column_set_resizable(col, TRUE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(completed_files_list), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    gtk_tree_view_column_add_attribute(col, renderer, "text",
                                       COMP_COL_OUTPUT_FILE);

    /* Next Column: Output File Name (no path) */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Output File");
    gtk_tree_view_column_set_visible(col, (show_full_paths) ? FALSE : TRUE);
    gtk_tree_view_column_set_resizable(col, TRUE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(completed_files_list), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    gtk_tree_view_column_add_attribute(col, renderer, "text",
                                       COMP_COL_OUTPUT_FILE_SHORT);

    /* Next Column: Pixbuf of output image */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "");
    gtk_tree_view_column_set_resizable(col, FALSE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(completed_files_list), col);
    renderer = gtk_cell_renderer_pixbuf_new();
    gtk_tree_view_column_pack_start(col, renderer, FALSE);
    gtk_tree_view_column_add_attribute(col, renderer, "pixbuf",
                                       COMP_COL_OUTPUT_THUMBNAIL);

    g_signal_connect (completed_files_list, "motion-notify-event",
                G_CALLBACK (completed_files_list_motion_notify_event_handler),
                NULL);

    g_signal_connect (completed_files_list, "leave-notify-event",
                G_CALLBACK (completed_files_list_leave_notify_event_handler),
                completed_files_list);

    g_signal_connect (completed_files_list, "scroll-event",
                G_CALLBACK (completed_files_list_scroll_event_handler),
                NULL);

    /* Next Column: Pixbuf of output image (larger version, for popup) */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "");
    gtk_tree_view_column_set_resizable(col, FALSE);
    gtk_tree_view_column_set_visible(col, FALSE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(completed_files_list), col);
    renderer = gtk_cell_renderer_pixbuf_new();
    gtk_tree_view_column_pack_start(col, renderer, FALSE);
    gtk_tree_view_column_add_attribute(col, renderer, "pixbuf",
                                       COMP_COL_OUTPUT_THUMBNAIL_BIG);

    /* Next Column: Current Status */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Status");
    gtk_tree_view_column_set_resizable(col, TRUE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(completed_files_list), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    gtk_tree_view_column_add_attribute(col, renderer, "text", COMP_COL_STATUS);

    /* Next Column: Log Info (hidden) */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Log Info");
    gtk_tree_view_column_set_visible(col, FALSE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(completed_files_list), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    gtk_tree_view_column_add_attribute(col, renderer, "text", COMP_COL_LOG);

    /* Next Column: Temporary Directory (hidden) */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Tmp Dir");
    gtk_tree_view_column_set_visible(col, FALSE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(completed_files_list), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    gtk_tree_view_column_add_attribute(col, renderer, "text",
                                       COMP_COL_TMP_DIR);

    /* Next Column: Layover/Shadow Mask File (hidden) */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Layover Mask");
    gtk_tree_view_column_set_visible(col, FALSE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(completed_files_list), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    gtk_tree_view_column_add_attribute(col, renderer, "text",
                                       COMP_COL_LAYOVER_SHADOW_MASK_FILE);

    /* Next Column: Clipped DEM File (hidden) */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Clipped DEM");
    gtk_tree_view_column_set_visible(col, FALSE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(completed_files_list), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    gtk_tree_view_column_add_attribute(col, renderer, "text",
                                       COMP_COL_CLIPPED_DEM_FILE);

    /* Next Column: Simulated SAR Image (hidden) */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Simulated SAR");
    gtk_tree_view_column_set_visible(col, FALSE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(completed_files_list), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    gtk_tree_view_column_add_attribute(col, renderer, "text",
                                       COMP_COL_SIMULATED_SAR_FILE);

    /* Next Column: Faraday rotations image (hidden) */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Faraday");
    gtk_tree_view_column_set_visible(col, FALSE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(completed_files_list), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    gtk_tree_view_column_add_attribute(col, renderer, "text",
                                       COMP_COL_FARADAY_FILE);

    /* Next Column: Entropy/Alpha Histogram (hidden) */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "E/A Hist");
    gtk_tree_view_column_set_visible(col, FALSE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(completed_files_list), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    gtk_tree_view_column_add_attribute(col, renderer, "text",
                                       COMP_COL_HIST_FILE);

    /* Next Column: Entropy/Alpha Classification Map (hidden) */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Class Map");
    gtk_tree_view_column_set_visible(col, FALSE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(completed_files_list), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    gtk_tree_view_column_add_attribute(col, renderer, "text",
                                       COMP_COL_CLASS_MAP_FILE);

    /* Next Column: Original Metadata File (hidden) */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Original Metadata");
    gtk_tree_view_column_set_visible(col, FALSE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(completed_files_list), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    gtk_tree_view_column_add_attribute(col, renderer, "text",
                                       COMP_COL_ORIGINAL_METADATA_FILE);

    /* Next Column: Metadata File (hidden) */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Metadata");
    gtk_tree_view_column_set_visible(col, FALSE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(completed_files_list), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    gtk_tree_view_column_add_attribute(col, renderer, "text",
                                       COMP_COL_METADATA_FILE);

    /* Next Column: Interferogram (hidden) */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Interferogram");
    gtk_tree_view_column_set_visible(col, FALSE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(completed_files_list), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    gtk_tree_view_column_add_attribute(col, renderer, "text",
                                       COMP_COL_INTERFEROGRAM);

    /* Next Column: Coherence (hidden) */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Coherence");
    gtk_tree_view_column_set_visible(col, FALSE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(completed_files_list), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    gtk_tree_view_column_add_attribute(col, renderer, "text",
                                       COMP_COL_COHERENCE);

    /* Next Column: Slave metadata (hidden) */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Slave metadata");
    gtk_tree_view_column_set_visible(col, FALSE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(completed_files_list), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    gtk_tree_view_column_add_attribute(col, renderer, "text",
                                       COMP_COL_SLAVE_METADATA);

    /* Next Column: Baseline (hidden) */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Baseline");
    gtk_tree_view_column_set_visible(col, FALSE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(completed_files_list), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    gtk_tree_view_column_add_attribute(col, renderer, "text",
                                       COMP_COL_BASELINE);

    /* Next Column: Incidence Angles (hidden) */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Incidence Angles");
    gtk_tree_view_column_set_visible(col, FALSE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(completed_files_list), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    gtk_tree_view_column_add_attribute(col, renderer, "text",
                                       COMP_COL_INCID_ANGLES_FILE);

    gtk_tree_view_set_model(GTK_TREE_VIEW(completed_files_list),
        GTK_TREE_MODEL(completed_list_store));

    g_object_unref(completed_list_store);

    gtk_tree_selection_set_mode(
        gtk_tree_view_get_selection(GTK_TREE_VIEW(completed_files_list)),
        GTK_SELECTION_MULTIPLE);
}

void
set_output_name(GtkTreeIter *iter, const gchar *name)
{
  gchar *basename = g_path_get_basename(name);
  gtk_list_store_set(list_store, iter,
                     COL_OUTPUT_FILE, name,
                     COL_OUTPUT_FILE_SHORT, basename,
                     -1);
}

void
refresh_file_names()
{
  GtkTreeView *in_files_view;
  GtkTreeView *completed_files_view;
  GtkTreeViewColumn *col;
  GtkWidget *in_files;
  GtkWidget *completed_files;

  int show_aux = have_ancillary_files_in_list() || have_meta_files_in_list();
  int show_polsarpro_aux = have_polsarpro_files_in_list();

  // Get tree views
  in_files = get_widget_checked("files_list");
  in_files_view = GTK_TREE_VIEW(in_files);
  completed_files = get_widget_checked("completed_files_list");
  completed_files_view = GTK_TREE_VIEW(completed_files);

  // Set input files column visibility to toggle between full-path file names
  // and short file names
  col = gtk_tree_view_get_column(in_files_view, COL_INPUT_FILE);
  gtk_tree_view_column_set_visible(col, show_full_paths);
  col = gtk_tree_view_get_column(in_files_view, COL_INPUT_FILE_SHORT);
  gtk_tree_view_column_set_visible(col, !show_full_paths);
  col = gtk_tree_view_get_column(in_files_view, COL_ALL_AUX_FILES);
  gtk_tree_view_column_set_visible(col, show_full_paths && show_aux);
  col = gtk_tree_view_get_column(in_files_view, COL_ALL_AUX_FILES_SHORT);
  gtk_tree_view_column_set_visible(col, !show_full_paths && show_aux);
  col = gtk_tree_view_get_column(in_files_view, COL_INPUT_FILE_SHORT);
  gtk_tree_view_column_set_visible(col, !show_full_paths);
  col = gtk_tree_view_get_column(in_files_view, COL_OUTPUT_FILE);
  gtk_tree_view_column_set_visible(col, show_full_paths);
  col = gtk_tree_view_get_column(in_files_view, COL_OUTPUT_FILE_SHORT);
  gtk_tree_view_column_set_visible(col, !show_full_paths);
  col = gtk_tree_view_get_column(in_files_view, COL_POLSARPRO_DISPLAY);
  gtk_tree_view_column_set_visible(col, show_polsarpro_aux);

  // Set completed files column visibility to toggle between full-path file names
  // and short file names
  col = gtk_tree_view_get_column(completed_files_view, COMP_COL_INPUT_FILE);
  gtk_tree_view_column_set_visible(col, show_full_paths);
  col = gtk_tree_view_get_column(completed_files_view, COMP_COL_INPUT_FILE_SHORT);
  gtk_tree_view_column_set_visible(col, !show_full_paths);
  col = gtk_tree_view_get_column(completed_files_view, COMP_COL_OUTPUT_FILE);
  gtk_tree_view_column_set_visible(col, show_full_paths);
  col = gtk_tree_view_get_column(completed_files_view, COMP_COL_OUTPUT_FILE_SHORT);
  gtk_tree_view_column_set_visible(col, !show_full_paths);
}

gchar * get_ancillary_file_from_input_list(const gchar *file_name)
{
  gchar * ancillary_file = NULL;

  if (!is_polsarpro(file_name)) return NULL;

  // For each file in the input list, check to see if it is the passed-in
  // input file, then return the associated ancillary file (even if missing
  // or blank)
  GtkTreeIter iter;
  gboolean more_items =
      gtk_tree_model_get_iter_first(GTK_TREE_MODEL(list_store), &iter);
  while (more_items) {
    int found=FALSE;
    gchar * input_file;
    gtk_tree_model_get(GTK_TREE_MODEL(list_store), &iter,
                       COL_INPUT_FILE, &input_file,
                       COL_ANCILLARY_FILE, &ancillary_file,
                       -1);
    if (strcmp(file_name, input_file) == 0) {
      // Found the right input file...
      // But don't exit loop yet, must g_free input_file first
      found = TRUE;
    }

    g_free(input_file);

    if (found)
      break;

    g_free(ancillary_file); // wasn't the right one, free it
    more_items = gtk_tree_model_iter_next(GTK_TREE_MODEL(list_store), &iter);
  }

  return ancillary_file;
}

gchar * get_meta_file_from_input_list(const gchar *file_name)
{
  gchar * meta_file = NULL;

  // For each file in the input list, check to see if it is the passed-in
  // input file, then return the associated ancillary file (even if missing
  // or blank)
  GtkTreeIter iter;
  gboolean more_items =
      gtk_tree_model_get_iter_first(GTK_TREE_MODEL(list_store), &iter);
  while (more_items) {
    int found=FALSE;
    gchar * input_file;
    gtk_tree_model_get(GTK_TREE_MODEL(list_store), &iter,
                       COL_INPUT_FILE, &input_file,
                       COL_METADATA_FILE, &meta_file,
                       -1);
    if (strcmp(file_name, input_file) == 0) {
      // Found the right input file...
      // But don't exit loop yet, must g_free() input_file
      found = TRUE;
    }

    g_free(input_file);

    if (found)
      break;

    g_free(meta_file);
    more_items = gtk_tree_model_iter_next(GTK_TREE_MODEL(list_store), &iter);
  }

  return meta_file;
}
