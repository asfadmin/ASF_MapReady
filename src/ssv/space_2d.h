// What do scientists and even more programmers do in 2D space?  They
// embed a dozen subtly differentiated coordinates systems and bore
// and cofuse the bejeebers out of themselves and even more out of
// their readers by constantly translating between them.  This class
// is supposed to help solve this problem by providing a way to
// register coordinate systems and handle translation between them
// automaticly.  Coordinate systems in this context can have
// restricted domains (to make catching errors easier).
// 
// References to registered coordinate systems are stored in an N-ary
// tree structure according to the reference systems in terms of which
// they are defined.  A system may be modified after it is registered,
// which alters the way traslation is performed between it and
// children of the node containing it and systems in other nodes.
// IMPROVEME: the rest of this paragraph is umimplemented: I believe
// this is usually the desired behavior in computer graphics at least,
// but it is also possible to specify new coordinate systems in terms
// of a system in a leaf node, but request that they be translated
// into the terms of some other system and stored under the node
// containing that other system.  For example, requesting that a
// system be stored under the root system makes it immune to changes
// in any other system (since the root system can't be redefined).
//
// Currently this class only handles scaling and translation since
// thats all I need at the moment.  It would be simple enough to
// extend it to handle other systems.  Another obvious omission is the
// lack of a fast batch translation interface: each translation
// involves a trip through the tree.
//
// This class can be used to perform explicit translation between
// coordinate systems using a method call.  This saves the reader of
// your code from having to deduce from a potentially convoluted
// translation expression which coordinate systems are involved: the
// method call says explicitly, and all they have to do is look at the
// point where those system are defined to see your nice comments
// explaining what they are and why they exist (hint hint :).
//
// As an alternative, a companion class (Coordinate2D or some such)
// the instances of which contain references to a Space2D instance and
// know which coordinate system they are defined in could be used to
// enable automatic translation inside routines that take Coordinate2D
// type instances as arguments.

#include <math.h>

#include <glib.h>

#ifndef SPACE_2D
#define SPACE_2D

// Helper type representing a single coordinate system in terms of an
// unspecified base system.  The data members of this structure are to
// be set by the user to specify a new system.  Twiddling the members
// of a system after it has been registered will (unless specific
// steps are taken) affect how subsequent translations involving that
// system or other systems defined in its terms are performed (this is
// a feature, see above).
typedef struct {
  // Origin of system in reference system.
  gdouble offset_x;
  gdouble offset_y;
  // Units in system are each the size of scale units in reference.
  gdouble scale_x;
  gdouble scale_y;
  // Extents of the coordinate system, in terms of the system itself.
  // It seems logical to me to specify the extent of the new system in
  // terms of itself, since the new system may actually by larger than
  // the reference system in terms of which it is defined.  These
  // members can have C99 macro values INFINITY or -INFINITY.
  gdouble min_x;
  gdouble min_y;
  gdouble max_x;
  gdouble max_y;
  // Private variable.
  gint reference_count;
} CoordinateSystem2D;

// Create a new coordinate system.
CoordinateSystem2D *
coordinate_system_2d_new (gdouble offset_x, gdouble offset_y,
			  gdouble scale_x, gdouble scale_y,
			  gdouble min_x, gdouble min_y,
			  gdouble max_x, gdouble max_y);

// Increment reference count of self, returning self as a convenience.
CoordinateSystem2D *
coordinate_system_2d_ref (CoordinateSystem2D *self);

// Decrement reference count of self, freeing self if reference count
// falls to zero.
void
coordinate_system_2d_unref (CoordinateSystem2D *self);

// All data members are private.
typedef struct {
  // Root of tree of CoordinateSystem2D instances.
  GNode *root;
  // Hash of CoordinateSystem2D pointers to the GNode instances that
  // contain them .  This just speeds up lookup of the tree nodes.
  GHashTable *systems;
  gint reference_count;
} Space2D;

// Create a new space with only a single root system defined.  The
// root system has its origin at (0, 0), scale factors of 1.0 in both
// directions,and infinite extents in both directions.
Space2D *
space_2d_new (void);

// Register a new coordinate system new_system defined in terms of
// reference, or in terms of the root system if reference is NULL.
// The extents of the new system may exceed those of its reference.
// The instance self takes ownership of new_system (i.e. the reference
// count of new_system is not incremented, but will be unref'ed when
// self is freed).
void
space_2d_add_system (Space2D *self, CoordinateSystem2D *reference, 
		     CoordinateSystem2D *new_system);

// Add a new system defined in terms of reference (see the add_system
// method description), but installed in the coordinate system tree
// with root as its parent.  This method allows new systems to be
// defined in terms of systems other than root, without their absolute
// meaning being subject to subsequent modification of those other
// systems.  The contents of new_system are modified by this method
// such that the coordinate system is expressed in terms of the root
// system.
void
space_2d_add_system_under_root (Space2D *self, CoordinateSystem2D *reference,
				CoordinateSystem2D *new_system);

// Translate point (c_x, c_y) from_system to_system.  Return true iff
// (c_x, c_y) translates to a point in the domain of to_system.  Note
// that if from_system and to_system are identical, nothing is done.
gboolean
space_2d_translate (Space2D *self, CoordinateSystem2D *from_system,
		    CoordinateSystem2D *to_system, gdouble *c_x,
		    gdouble *c_y);

// Unregisters system with self.  It is an error if system is not
// registered with self or has other registered systems defined in its
// terms (i.e. if it is not a leaf node in the tree of registered
// systems).
void
space_2d_remove_system (Space2D *self, CoordinateSystem2D *system);

// Increment reference count of self, returning self as a convenience.
Space2D *
space_2d_ref (Space2D *self);

// Decrement reference count of self, freeing self if reference count
// falls to zero.
void
space_2d_unref (Space2D *self);

#endif // SPACE_2D
