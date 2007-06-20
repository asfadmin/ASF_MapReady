// Implementation of the interface described in space_2d.h.

#include "space_2d.h"

CoordinateSystem2D *
coordinate_system_2d_new (gdouble offset_x, gdouble offset_y,
			  gdouble scale_x, gdouble scale_y,
			  gdouble min_x, gdouble min_y,
			  gdouble max_x, gdouble max_y)
{
  CoordinateSystem2D *self = g_new0 (CoordinateSystem2D, 1);
  
  self->offset_x = offset_x;
  self->offset_y = offset_y;
  self->scale_x = scale_x;
  self->scale_y = scale_y;
  self->min_x = min_x;
  self->min_y = min_y;
  self->max_x = max_x;
  self->max_y = max_y;

  self->reference_count = 1;

  return self;
}

CoordinateSystem2D *
coordinate_system_2d_ref (CoordinateSystem2D *self)
{
  self->reference_count++;

  return self;
}

void
coordinate_system_2d_unref (CoordinateSystem2D *self)
{
  self->reference_count--;
  
  if ( self->reference_count == 0 ) {
    g_free (self);
  }
}

Space2D *
space_2d_new (void)
{
  Space2D *self = g_new (Space2D, 1);

  CoordinateSystem2D *root_system
    = coordinate_system_2d_new (0.0, 0.0, 1.0, 1.0, -INFINITY, -INFINITY,
				INFINITY, INFINITY);

  self->root = g_node_new (root_system);

  self->systems = g_hash_table_new_full (g_direct_hash, g_direct_equal,
					 NULL, NULL);

  return self;
}

void
space_2d_add_system (Space2D *self, CoordinateSystem2D *reference, 
		     CoordinateSystem2D *new_system)
{
  GNode *reference_node;

  if ( reference == NULL ) {
    reference_node = self->root;
  }
  else {
    reference_node = g_hash_table_lookup (self->systems, reference);
    g_assert (reference_node != NULL);
  }

  GNode *new_node
    = g_node_append_data (reference_node,
			  coordinate_system_2d_ref (new_system));

  g_hash_table_insert (self->systems, new_system, new_node);
}

void
space_2d_add_system_under_root (Space2D *self, CoordinateSystem2D *reference,
				CoordinateSystem2D *new_system)
{
  // Sigh... my proverbial eyes are bigger than my stomach.  I don't
  // actually need this functionality myself yet, so it isn't
  // implemented.
  g_assert_not_reached ();
}

// Translate point (c_x, c_y) from_system to_system.  Return true iff
// (c_x, c_y) are in the domain of to_system.
gboolean
space_2d_translate (Space2D *self, CoordinateSystem2D *from_system,
		    CoordinateSystem2D *to_system, gdouble *c_x, gdouble *c_y)
{
  // Our strategy is to translate from_system up to an ancestor common
  // with to_system, then back down to to_system.  We record the path
  // up from to_system on a stack so we can find our way back down
  // again.

  // Look up the nodes of from_system and to_system.
  GNode *from_node = g_hash_table_lookup (self->systems, from_system);
  GNode *to_node = g_hash_table_lookup (self->systems, to_system);

  // Find the common ancestor.

  GNode *cfn = from_node;    // Current node in from_node path.
  GNode *ctn = to_node;	     // Current node in to_node path.
  // Nodes in path from to_node to node below common ancestor.
  GQueue *to_path = g_queue_new ();

  // First we walk up the path of the "lower" node until we reach a
  // point no deeper than the deeper node.
  while ( g_node_depth (cfn) > g_node_depth (ctn) ) {
    cfn = cfn->parent;
  }
  while ( g_node_depth (ctn) > g_node_depth (cfn) ) {
    g_queue_push_tail (to_path, ctn);
    ctn = ctn->parent;
  }
  // Now we move up to the ancestor of one of our two subjects at a
  // time while the two current positions are not pointing to the same
  // node.
  guint counter = 0;		// Counter for detecting odd/even.
  while ( cfn != ctn ) {
    if ( counter % 2 == 0 ) {
      cfn = cfn->parent;
      g_assert (cfn != NULL);	// At least root should be in common.
    }
    else {
      g_queue_push_tail (to_path, ctn);
      ctn = ctn->parent;
      g_assert (ctn != NULL);   // At least root should be in common.
    }
    counter++;
  }
  GNode *common_ancestor = cfn;
  g_assert (common_ancestor == ctn);

  // IMPROVEME: obviously we aren't making any effort to do things in
  // a clever way to avoid error buildup here.

  // Translate back up to the common node.
  for ( cfn = from_node ; cfn != common_ancestor ; cfn = cfn->parent ) {
    CoordinateSystem2D *cs = cfn->data;	   // Current system.
    *c_x = (*c_x + cs->offset_x) * cs->scale_x;
    *c_y = (*c_y + cs->offset_y) * cs->scale_y;
  }
  
  // Translate back down to to_node.
  for ( ctn = g_queue_pop_tail (to_path) ; ctn != NULL ;
	ctn = g_queue_pop_tail (to_path) ) {
    CoordinateSystem2D *cs = ctn->data;	   // Current system.
    *c_x = (*c_x - cs->offset_x ) / cs->scale_x;
    *c_y = (*c_y - cs->offset_y) / cs->scale_y;
  }
  
  g_queue_free (to_path);

  if ( *c_x >= to_system->min_x && *c_x <= to_system->max_x
       && *c_y >= to_system->min_y && *c_y <= to_system->max_y ) {
    return TRUE;
  }
  else {
    return FALSE;
  }
}

void
space_2d_remove_system (Space2D *self, CoordinateSystem2D *system)
{
  GNode *system_node = g_hash_table_lookup (self->systems, system);
  g_assert (system_node != NULL);
  g_assert (system_node->data == system);
  g_assert (G_NODE_IS_LEAF (system_node));

  coordinate_system_2d_unref (system);
  g_node_destroy (system_node);
}

// Increment reference count of self, returning self as a convenience.
Space2D *
space_2d_ref (Space2D *self)
{
  self->reference_count++;

  return self;
}

// Tree traversal function which unrefs all the coordinate systems
// stored in the tree.
gboolean
unref_system_traverse_func (GNode *node, gpointer data)
{
  coordinate_system_2d_unref (node->data);
  node->data = NULL;
  
  return FALSE;
}

// Decrement reference count of self, freeing self if reference count
// falls to zero.
void
space_2d_unref (Space2D *self)
{
  self->reference_count--;
  
  if ( self->reference_count == 0 ) {
    g_node_traverse (self->root, G_IN_ORDER, G_TRAVERSE_ALL, -1,
		     unref_system_traverse_func, NULL);
    g_node_destroy (self->root);
    g_hash_table_destroy (self->systems);
    g_free (self);
    self = NULL;
  }
}
