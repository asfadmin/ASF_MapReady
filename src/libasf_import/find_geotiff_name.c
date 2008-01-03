// Implementation of interface described in find_geotiff_name.h.

#include "find_geotiff_name.h"

GString *
find_geotiff_name (const char *inBaseName)
{
  // Possible TIFF file names as described in interface.
    GString *p0 = g_string_new(inBaseName);
    GString *p1 = g_string_append (g_string_new (inBaseName), ".tif");
    GString *p2 = g_string_append (g_string_new (inBaseName), ".tiff");
    GString *p3 = g_string_append (g_string_new (inBaseName), ".TIF");
    GString *p4 = g_string_append (g_string_new (inBaseName), ".TIFF");

  // Result to return.
    GString *result = NULL;

  // Find first possibility which exists.
    if ( g_file_test (p1->str, G_FILE_TEST_EXISTS) ) {
        result = p1;
        g_string_free (p0, TRUE);
        g_string_free (p2, TRUE);
        g_string_free (p3, TRUE);
        g_string_free (p4, TRUE);
    }
    else if ( g_file_test (p2->str, G_FILE_TEST_EXISTS) ) {
        result = p2;
        g_string_free (p0, TRUE);
        g_string_free (p1, TRUE);
        g_string_free (p3, TRUE);
        g_string_free (p4, TRUE);
    }
    else if ( g_file_test (p3->str, G_FILE_TEST_EXISTS) ) {
        result = p3;
        g_string_free (p0, TRUE);
        g_string_free (p1, TRUE);
        g_string_free (p2, TRUE);
        g_string_free (p4, TRUE);
    }
    else if ( g_file_test (p4->str, G_FILE_TEST_EXISTS) ) {
        result = p4;
        g_string_free (p0, TRUE);
        g_string_free (p1, TRUE);
        g_string_free (p2, TRUE);
        g_string_free (p3, TRUE);
    }
    else if ( g_file_test (p0->str, G_FILE_TEST_EXISTS) ) {
        result = p0;
        g_string_free (p1, TRUE);
        g_string_free (p2, TRUE);
        g_string_free (p3, TRUE);
        g_string_free (p4, TRUE);
    }

    return result;
}
