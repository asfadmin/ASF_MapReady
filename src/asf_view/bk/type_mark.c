// Implementation of interface described in type_mark.h.

#include <string.h>

#include "type_mark.h"

static gboolean
is_valid_gtype_char (gchar arg)
{
  if ( (arg >= 'a' && arg <= 'z') || (arg >= 'A' && arg <= 'Z') ) {
    return TRUE;
  }
  if ( arg >= '0' && arg <= '9' ) {
    return TRUE;
  }
  if ( (arg == '-') || (arg == '_') || (arg == '+') ) {
    return TRUE;
  }

  return FALSE;
}

static GNode *
parse (const gchar *spec)
{
  gsize len = strlen (spec);

  g_assert (sizeof (size_t) >= sizeof (guint));
  g_assert (len <= (size_t) UINT_MAX);

  guint ii;
  guint first_nonspace = -1, end_root_type = -1;
  guint first_bracket = -1, last_bracket = -1;
  gint open_bracket_count = 0;
  // Positions of commas that are only one bracket level deep.
  GArray *commas = g_array_new (FALSE, FALSE, sizeof (guint));
  for ( ii = 0 ; ii < len ; ii++ ) {
    if ( ! g_ascii_isspace (spec[ii]) ) {
      first_nonspace = ii;
    }
    else if ( first_nonspace != -1 && g_ascii_isspace (spec[ii]) ) {
      end_root_type = ii; // Index of character one past end of root type.
    }
    if ( spec[ii] == '[' ) {
      g_assert (first_nonspace != -1);
      g_assert (end_root_type != -1);
      if ( first_bracket == -1 ) {
	first_bracket = ii;
      }
      open_bracket_count++;
    }
    else if ( spec[ii] == ']' ) {
      g_assert (first_nonspace != -1);
      g_assert (end_root_type != -1);
      g_assert (first_bracket != -1);
      last_bracket = ii;
      open_bracket_count--;
      g_assert (open_bracket_count >= 0);
    }
    else if ( spec[ii] == ',' ) {
      g_assert (first_nonspace != -1);
      g_assert (end_root_type != -1);
      g_assert (first_bracket != -1);
      if ( open_bracket_count == 1 ) {
	g_array_append_val (commas, ii);
      }
    }
    else {
      // Forbid characters which aren't in our grammar or allowed in
      // legal GType names.
      if ( ! is_valid_gtype_char (spec[ii]) ) {
	GString *invalid_char = g_string_new ("");
	g_string_append_c (invalid_char, spec[ii]);
	g_error ("invalid character '%s' in GType name string",
		 invalid_char->str);
	g_string_free (invalid_char, TRUE);
      }
    }
  }

  // Allocate the root of the tree, initialized with the root type.
  gchar *root_type_name
    = g_strndup (spec + first_nonspace, end_root_type - end_root_type);
  GType *root_type = g_new (GType, 1);
  *root_type = g_type_from_name (root_type_name);
  GNode *result = g_node_new (root_type);
  g_free (root_type_name);

  // Allocate all the subtrees.
  gchar *cts;   // Current comma-delimited type string.
  if ( commas->len == 0 ) {
    cts = g_strndup (spec + first_bracket + 1,
		     last_bracket - first_bracket - 1);
    g_strstrip (cts);
    GNode *child = parse (cts);
    g_node_append (result, child);
  }
  for ( ii = 0 ; ii <= commas->len ; ii++ ) {
    // Special handling for the type before the first comma.
    if ( ii == 0 ) {
      cts = g_strndup (spec + first_bracket + 1,
		       g_array_index (commas, guint, ii) - first_bracket - 1);
    }
    // Special handling for the type after the first comma.
    else if ( ii == commas->len ) {
      guint lci = g_array_index (commas, guint, ii - 1);
      cts = g_strndup (spec + lci + 1, last_bracket - lci - 1);
    }
    // The other types in between.
    else {
      guint fc = g_array_index (commas, guint, ii - 1);
      guint lc = g_array_index (commas, guint, ii);
      cts = g_strndup (spec + fc + 1, lc - fc - 1);
    }
    g_strstrip (cts);
    GNode *child = parse (cts);
    g_node_append (result, child);
  }

  return result;
}

TM *
tm_new (const char *spec)
{
  // All this is untested.
  g_assert_not_reached ();

  TM *self = g_new (TM, 1);

  self->root = parse (spec);

  return self;
}

TM *
tm_new_from_type_and_list (GType root_type, const char *children)
{
  const gchar *root_type_name = g_type_name (root_type);
  g_assert (root_type_name != NULL);

  GString *total_type = g_string_new (root_type_name);
  g_string_append_c (total_type, '[');
  g_string_append (total_type, children);
  g_string_append_c (total_type, ']');
  
  TM *self = tm_new (total_type->str);

  g_string_free (total_type, TRUE);

  return self;
}

// Compare the type marks in the trees.  This is not quite a symmetric
// comparison: it implements the behavior that allows unmatched blank
// (NULL data) fields in self, but not in other (see the tm_check
// description in the interface).
static gboolean
compare_trees (GNode *self, GNode *other)
{
  if ( self->data == NULL ) {
    return TRUE;
  }
  else {
    if ( other->data == NULL ) {
      return FALSE;
    }
    if ( *((GType *) self->data) != *((GType *) other->data) ) {
      return FALSE;
    }
    GNode *sc = g_node_first_child (self), *oc = g_node_first_child (other);
    while ( sc != NULL && oc != NULL ) {
      if ( ! compare_trees (sc, oc) ) {
	return FALSE;
      }
      sc = g_node_next_sibling (sc);
      oc = g_node_next_sibling (oc);
    }
    if ( ! (sc == NULL && oc == NULL) ) {
      return FALSE;
    }
    
    return TRUE;
  }
}

gboolean
tm_check (TM *self, TM *other)
{
  return compare_trees (self->root, other->root);
}

// Free type mark.
void
tm_free (TM *self)
{
  g_node_destroy (self->root);

  g_free (self);
}
