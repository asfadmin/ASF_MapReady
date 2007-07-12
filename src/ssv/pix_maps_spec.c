// Implementation of the interface described in pix_maps_spec.h.

#include "pix_maps_spec.h"

PixMapsSpec *
pix_maps_spec_new (void)
{
  PixMapsSpec *self = g_new (PixMapsSpec, 1);

  self->map_specs = g_ptr_array_new ();

  self->half_popularity = PIX_MAPS_SPEC_DEFAULT_POPULARITY_RANGE_SIZE / 2;

  self->reference_count = 1;

  return self;
}

void
pix_maps_spec_add_map (PixMapsSpec *self, gdouble start, gdouble end,
		      gdouble new_value)
{
  if ( isnan (start) || isnan (end) ) {
    g_assert (isnan (start) && isnan (end));
  }
  else {
    g_assert (start <= end);
  }

  // Make sure that neither endpoint of the new layer falls in any
  // other map range.
  guint ii;
  if ( ! isnan (start) ) {
    // For a normal range, we test that we aren't in another range.
    for ( ii = 0 ; ii < self->map_specs->len ; ii++ ) {
      MapSpec *cs = g_ptr_array_index (self->map_specs, ii);
      g_assert (!(start >= cs->start && start <= cs->end));
      g_assert (!(end >= cs->end && end <= cs->end));
    }
  }
  else {
    // For a NAN (non)range, we test that there aren't any other NAN
    // (non)ranges already installed.
    for ( ii = 0 ; ii < self->map_specs->len ; ii++ ) {
      MapSpec *cs = g_ptr_array_index (self->map_specs, ii);
      g_assert (! isnan (cs->start));
    }
  }

  MapSpec *new_map = g_new (MapSpec, 1);

  new_map->start = start;
  new_map->end = end;
  new_map->new_value = new_value;
  // New maps start out unpopular and have to prove themselves.
  new_map->popularity = -self->half_popularity;

  g_ptr_array_add (self->map_specs, new_map);
}

guint
pix_maps_spec_map_count (PixMapsSpec *self)
{
  return self->map_specs->len;
}

void
pix_maps_spec_set_popularity_range_size (PixMapsSpec *self, guint size)
{
  // Must use 'less than' to allow for positive/negative asymmetry.
  g_assert (size / 2 < INT_MAX);

  self->half_popularity = size / 2;

  guint ii;
  for ( ii = 0 ; ii < self->map_specs->len ; ii++ ) {
    MapSpec *cs = g_ptr_array_index (self->map_specs, ii);
    cs->popularity = -self->half_popularity;
  }
}

static gint
compare_map_popularities (gconstpointer a, gconstpointer b)
{
  const MapSpec *ma = a, *mb = b;

  if ( ma->popularity < mb->popularity ) {
    return -1;
  }
  else if ( ma->popularity > mb->popularity ) {
    return 1;
  }
  else {
    return 0;
  }
}

// Shift the popularity spectrum down, and compress it downward, to
// make room for more increments.  Doing this regularly fails to
// preserve real statistics about the relative popularity of the maps,
// but it preserves order which is good enough for us.
static void
depopularize_all_maps (PixMapsSpec *self)
{
  guint ii;
  gint min_pop = INT_MAX, max_pop = INT_MIN;
  for ( ii = 0 ; ii < self->map_specs->len ; ii++ ) {
    MapSpec *cm = g_ptr_array_index (self->map_specs, ii); // Current map.
    if ( cm->popularity < min_pop ) {
      min_pop = cm->popularity;
      g_assert (min_pop >= -self->half_popularity);
    }
    if ( cm->popularity > max_pop ) {
      max_pop = cm->popularity;
      g_assert (max_pop <= self->half_popularity);
    }
  }

  for ( ii = 0 ; ii < self->map_specs->len ; ii++ ) {
    MapSpec *cm = g_ptr_array_index (self->map_specs, ii); // Current map.

    glong cp = cm->popularity;

    // Shift all popularities to the bottom of the available range.
    cp -= min_pop - (-self->half_popularity);
    // Then compress the range of popularities by two.
    cp -= (cp - (-self->half_popularity)) / 2;

    g_assert (cp < INT_MAX);
    cm->popularity = cp;
  }

  // Increment the popularity of the most popular entry by one to make
  // sure it stays in the lead in the face of the above code.
  MapSpec *first_map = g_ptr_array_index (self->map_specs, 0);
  first_map->popularity++;

  // Sort the maps by popularity.
  g_ptr_array_sort (self->map_specs, compare_map_popularities);
}

gdouble
pix_maps_spec_map (PixMapsSpec *self, gdouble value)
{
  guint ii;

  g_assert (self != NULL);
  g_assert (self->map_specs != NULL);

  for ( ii = 0 ; ii < self->map_specs->len ; ii++ ) {

    MapSpec *cm = g_ptr_array_index (self->map_specs, ii);

    if ( isnan (value) ) {
      if ( isnan (cm->start) ) {
	g_assert (isnan (cm->end));
	if ( G_UNLIKELY (cm->popularity == self->half_popularity) ) {
	  depopularize_all_maps (self);
	}
	cm->popularity++;
	return cm->new_value;
      }
    }

    else if ( value >= cm->start && value <= cm->end ) {
      if ( G_UNLIKELY (cm->popularity == self->half_popularity) ) {
	depopularize_all_maps (self);
      }
      cm->popularity++;
      return cm->new_value;
    }
  }

  return value;
}

GString *
pix_maps_spec_freeze_as_gstring (PixMapsSpec *self)
{
  GString *result = g_string_new ("");

  g_string_append_printf (result, "%u", self->map_specs->len);

  guint ii;
  for ( ii = 0 ; ii < self->map_specs->len ; ii++ ) {
    g_string_append_c (result, ' ');
    MapSpec *cm = g_ptr_array_index (self->map_specs, ii);    
    g_string_append_printf (result, "%lf %lf %lf %d",
			    cm->start, cm->end, cm->new_value, cm->popularity);
  }

  return result;
}

PixMapsSpec *
pix_maps_spec_ref (PixMapsSpec *self)
{
  self->reference_count++;

  return self;
}

void
pix_maps_spec_unref (PixMapsSpec *self)
{
  self->reference_count--;

  if ( self->reference_count == 0 ) {
    guint ii;
    for ( ii = 0 ; ii < self->map_specs->len ; ii++ ) {
      g_free (g_ptr_array_index (self->map_specs, ii));
    }
    g_ptr_array_free (self->map_specs, TRUE);
    g_free (self);
  }
}
