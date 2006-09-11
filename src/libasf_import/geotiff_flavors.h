// The GeoTIFF format is probably best viewed as defining a family of
// closely related formats.  We call the 'flavors'.  Our strategy is
// to try to detect which flavor we're actually dealing with, so we
// have a better chance of importing it correctly.  Note that if we
// get the wrong type, it actually may not matter, since the fact that
// we got a certain notion of a GeoTIFF's flavor may mean that flavor
// is close enough to what we have that things will work.

#ifndef GEOTIFF_FLAVORS_H
#define GEOTIFF_FLAVORS_H

// Type of a pointer to a function appropriate for importing a
// geotiff_file of some specific flavor.
typedef void (*geotiff_importer)(const char *geotiff_file, 
                                 const char *outBaseName);

// Try to detect the flavor of a given geotiff file, and return an
// appropriate geotiff_importer.
geotiff_importer
detect_geotiff_flavor (const char *file);

#endif // GEOTIFF_FLAVORS_H
