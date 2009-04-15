// Implementation of interface described in find_arcgis_geotiff_aux_name.h.

#include "find_arcgis_geotiff_aux_name.h"

GString *
find_arcgis_geotiff_aux_name (const char *inBaseName)
{
  // Possible TIFF file names as described in interface.
  GString *p1 = g_string_append (g_string_new (inBaseName), ".tif.aux");
  GString *p2 = g_string_append (g_string_new (inBaseName), ".tiff.aux");
  GString *p3 = g_string_append (g_string_new (inBaseName), ".aux");
  GString *p4 = g_string_append (g_string_new (inBaseName), ".TIF.AUX");
  GString *p5 = g_string_append (g_string_new (inBaseName), ".TIFF.AUX");
  GString *p6 = g_string_append (g_string_new (inBaseName), ".AUX");

  // Result tp return.
  GString *result = NULL;

  // Find first possibility which exists.
  if ( fileExists (p1->str) ) {
    result = p1; 
    g_string_free (p2, TRUE);
    g_string_free (p3, TRUE);
    g_string_free (p4, TRUE);
    g_string_free (p5, TRUE);
    g_string_free (p6, TRUE);
  }
  else if ( fileExists (p2->str) ) {
    result = p2;
    g_string_free (p1, TRUE);
    g_string_free (p3, TRUE);
    g_string_free (p4, TRUE);
    g_string_free (p5, TRUE);
    g_string_free (p6, TRUE);
  }
  else if ( fileExists (p3->str) ) {
    result = p3;
    g_string_free (p1, TRUE);
    g_string_free (p2, TRUE);
    g_string_free (p4, TRUE);
    g_string_free (p5, TRUE);
    g_string_free (p6, TRUE);
  }
  else if ( fileExists (p4->str) ) {
    result = p4;
    g_string_free (p1, TRUE);
    g_string_free (p2, TRUE);
    g_string_free (p3, TRUE);
    g_string_free (p5, TRUE);
    g_string_free (p6, TRUE);
  }
  else if ( fileExists (p5->str) ) {
    result = p5;
    g_string_free (p1, TRUE);
    g_string_free (p2, TRUE);
    g_string_free (p3, TRUE);
    g_string_free (p4, TRUE);
    g_string_free (p6, TRUE);
  }
  else if ( fileExists (p6->str) ) {
    result = p6;
    g_string_free (p1, TRUE);
    g_string_free (p2, TRUE);
    g_string_free (p3, TRUE);
    g_string_free (p4, TRUE);
    g_string_free (p5, TRUE);
  }

  return result;
}
