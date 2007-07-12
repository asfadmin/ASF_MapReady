// A type mark capable of representing a compound type (as in Eiffel
// PAIR[STRING, HASH[INT]], for example).  The individual type names
// should be those returned by G_OBJECT_TYPE_NAME() for an instance of
// the appropriate type, or "NULL" to indicate the a component type is
// unspecified (perhaps because it doesn't have a type in the GObject
// sense).

#include <glib.h>
#include <glib-object.h>

typedef struct {
  GNode *root;
} TM;

// Create a new type mark from a specification like "A[B[D], C[D, E]]"
// where the actual type are those returned by G_OBJECT_TYPE_NAME, or
// "NULL" (see above).  The odds are good that the
// tm_new_from_type_and_list is the method you really want (in fact
// this method exists mainly for comprehensibility).
TM *
tm_new (const char *spec);

// Like tm_new, but the outermost type and its associated brackets are
// omitted and a GType specified in their place.  This allows client
// classes to accept a non-redundant type specification (because if
// you are creating a TREE, then ROOT is of course going to be TREE).
// The spec argument provided with the above tm_new method would
// become just "B[D], C[D, E]", and the GType of A would be passed as
// the first argument.
TM *
tm_new_from_type_and_list (GType root, const char *children);

// Verify that other is of compound type self.  Actual types in other
// match NULL values in self, but not the other way around (i.e. we
// insist that other be at least as specific about itself as self is
// about itself).
gboolean
tm_check (TM *self, TM *other);

// Free type mark.
void
tm_free (TM *self);

