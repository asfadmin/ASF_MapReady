// A datatype which records how certain ranges of values in an image
// are to be remapped for display purposes.  The ranges specified by
// the map must not overlap.  The somewhat odd possibility of one
// range mapping into a different range (or into a point in itself)
// remains.  It is allowed for a range to have one or more endpoints
// at +/- 'inf' (meaning +/- infinity).  It is allowed for a range to
// have endpoints NAN and NAN, and remap pixels to NAN.

#ifndef PIX_MAPS_SPEC_H
#define PIX_MAPS_SPEC_H

#include <math.h>

#include <glib.h>

// A single map specification, and its popularity (which is only
// meaningful relatve to other MapSpec instances).  This is an
// internal type which shouldn't be used by clients.
typedef struct {
  gdouble start;
  gdouble end;
  gdouble new_value;
  gint popularity;
} MapSpec;

// All members are private.
typedef struct {
  GPtrArray *map_specs;		// Individual maps.
  // Popularities are constrained to [-half_popularity, half_popularity].
  gint half_popularity;	
  int reference_count;
} PixMapsSpec;

// See comments on the set_popularity_range_size method below.
#define PIX_MAPS_SPEC_DEFAULT_POPULARITY_RANGE_SIZE 10001

// Create a new empty instance with the default popularity range size.
PixMapsSpec *
pix_maps_spec_new (void);

// Add a new mapping to the instance.  It is allowed for start and end
// to be NAN, but if either is, both must be.  Otherwise, start must
// be less than or equal to end.  It is allowed for either or both
// endpoints to be +- 'inf' (meaning infinity).  New maps start out as
// unpopular as possible.  In general it is required that
//  
//      start <= end
// 
// but this requirment doesn't apply if both are NAN.
void 
pix_maps_spec_add_map (PixMapsSpec *self, gdouble start, gdouble end,
		      gdouble map_value);

// How many maps are currently in self?
guint
pix_maps_spec_map_count (PixMapsSpec *self);

// Set the approximate total range of popularities which may exist,
// and reset all range popularities to be equal.  This essentially
// specifies the temporal locality of the popularity contest between
// the different maps.  For example, if the popularity range is 100,
// then the maximum popularity lead one range will have over the
// others is only approximately 100, so it only takes 100 hits on a
// different range in calls to the map method to change the order in
// which ranges are checked.  In reality there is some compression of
// popularities that occurs when a range would otherwise exceed the
// maximum popularity, but the above is a decent representation of
// what goes on.
void
pix_maps_spec_set_popularity_range_size (PixMapsSpec *self, guint size);

// Try all the map entries (if any), returning the new value (which
// may equal the old value).  Values are tried approximately in order
// of decreasing recent popularity (the number of times
gdouble
pix_maps_spec_map (PixMapsSpec *self, gdouble value);

// Return a representation of the instance in a new GString (without
// preserving reference_count).
GString *
pix_maps_spec_freeze_as_gstring (PixMapsSpec *self);

// Increment reference count of self, returning a pointer to self as a
// convenience.
PixMapsSpec *
pix_maps_spec_ref (PixMapsSpec *self);

// Decrement reference count of self, freeing self if reference count
// falls to zero.
void
pix_maps_spec_unref (PixMapsSpec *self);

#endif // PIX_MAPS_SPEC_H
