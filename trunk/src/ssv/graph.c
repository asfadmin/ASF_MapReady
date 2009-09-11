// Implementation of interface described in graph.h.

#include <stdio.h>

#include "graph.h"
#include "utilities.h"

static GraphNode *
graph_node_new (gpointer contents)
{
  GraphNode *self = g_new (GraphNode, 1);
  self->in_edges = g_ptr_array_new ();
  self->out_edges = g_ptr_array_new ();
  self->contents = contents;

  return self;
}

Graph *
graph_new (void)
{
  Graph *self = g_new (Graph, 1);
  self->nodes = g_ptr_array_new ();
  self->edges = g_ptr_array_new ();
  self->reference_count = 1;

  return self;
}

GraphNode *
graph_add_node (Graph *self, gpointer contents)
{
  GraphNode *new_node = graph_node_new (contents);
  g_ptr_array_add (self->nodes, new_node);

  return new_node;
}

GraphEdge *
graph_add_edge (Graph *self, GraphNode *from_node, GraphNode *to_node,
		gpointer contents)
{
  GraphEdge *new_edge = g_new (GraphEdge, 1);
  new_edge->from_node = from_node;
  new_edge->to_node = to_node;
  new_edge->contents = contents;

  g_ptr_array_add (from_node->out_edges, new_edge);
  g_ptr_array_add (to_node->in_edges, new_edge);
  g_ptr_array_add (self->edges, new_edge);

  return new_edge;
}

void
graph_remove_edge (Graph *self, GraphEdge *edge)
{
  gboolean found_edge = g_ptr_array_remove_fast (self->edges, edge);
  g_assert (found_edge);
  found_edge = g_ptr_array_remove_fast (edge->from_node->out_edges, edge);
  g_assert (found_edge);
  found_edge = g_ptr_array_remove_fast (edge->to_node->in_edges, edge);
  g_assert (found_edge);
  g_free (edge);
}

gboolean
graph_try_remove_edge (Graph *self, GraphNode *from_node, GraphNode *to_node)
{
  guint ii;
  for ( ii = 0 ; ii < from_node->out_edges->len ; ii++ ) {
    GraphEdge *current_edge = g_ptr_array_index (from_node->out_edges, ii);
    if ( current_edge->to_node == to_node ) {
      graph_remove_edge (self, current_edge);
      return TRUE;
    }
  }

  return FALSE;
}

void
graph_remove_node (Graph *self, GraphNode *node)
{
  // IMPROVEME: for symmetry it might be nice to have a
  // graph_node_free method.
  g_assert (node->in_edges->len == 0 && node->out_edges->len == 0);
  g_ptr_array_free (node->out_edges, TRUE);
  g_ptr_array_free (node->in_edges, TRUE);
  gboolean found_node = g_ptr_array_remove_fast (self->nodes, node);
  g_free (node);
  g_assert (found_node);
}

// Detmine if subject is reachable from start_node using a depth-first
// search.  If path is non-NULL, return in it a new GQueue (owned by
// the caller) of GraphEdge pointers (owned by self) that make up the
// path found.  The head of path is the edge into subject, and the
// tail is the edge from start_node.
gboolean
graph_depth_first_search_for_node (Graph *self, GraphNode *start_node,
				   GraphNode *subject, GQueue **path)
{
  // IMPROVEME: this implementation isn't as efficient as it could be
  // (recompares things that are known not to be the desired node,
  // remarks already marked nodes).

  // Flags true iff we have found the subject, or the search has
  // failed, respectively.
  gboolean found = FALSE, hopeless = FALSE;

  // Existence hash which we use to mark nodes we are finished with.
  GHashTable *visited = g_hash_table_new (g_direct_hash, g_direct_equal);

  // Queue for backtracking in depth first search.
  GQueue *parents = g_queue_new ();

  // Keep a queue of the edges in the path currently being considered,
  // in case the user cares.
  GQueue *edges = g_queue_new ();

  GraphNode *current = start_node;

  while ( !found && !hopeless ) {
    if ( current == subject ) {
      found = TRUE;
    }
    else {
      // Mark (or remark, no harm done) the current node as visited.
      g_hash_table_insert (visited, current, NULL);
      gboolean eligible_child = FALSE;
      GraphEdge *edge = NULL;
      guint ii;
      for ( ii = 0 ; ii < current->out_edges->len ; ii++ ) {
	edge = g_ptr_array_index (current->out_edges, ii);
	if ( !my_g_hash_table_entry_exists (visited, edge->to_node) ) {
	  eligible_child = TRUE;
	  break;
	}
      }
      if ( eligible_child ) {
	g_queue_push_head (parents, current);
	g_queue_push_head (edges, edge);
	current = edge->to_node;
      }
      else {
	current = g_queue_pop_head (parents);
	g_queue_pop_head (edges);
	if ( current == NULL ) {
	  found = FALSE;
	  hopeless = TRUE;
	}
      }
    }
  }

  g_queue_free (parents);

  if ( path != NULL ) {		// If user is interested in the path...

    // Uncomment the following block to sanity check the path.
    /*
    if ( found ) {		// If we found a path...
      
      GraphEdge *head = g_queue_peek_head (edges);
      guint ii;
      gboolean head_promise = FALSE;
      for ( ii = 0 ; ii < subject->in_edges->len ; ii++ ) {
	if ( g_ptr_array_index (subject->in_edges, ii) == head ) {
	  head_promise = TRUE;
	  break;
	}
      }
      g_assert (head_promise);
      GraphEdge *tail = g_queue_peek_tail (edges);
      gboolean tail_promise;
      for ( ii = 0 ; ii < start_node->out_edges->len ; ii++ ) {
	if ( g_ptr_array_index (start_node->out_edges, ii) == tail ) {
	  tail_promise = TRUE;
	  break;
	}
      }
      g_assert (tail_promise);
      
      // Lets actually walk the path once to make sure things worked.
      GraphNode *cn = subject;
      for ( ii = 0 ; cn != start_node && ii < edges->length ; ii++ ) {
	// IMPROVEME: this is inefficient for long paths, oh well its
	// only debug code.
	GraphEdge *ce = g_queue_peek_nth (edges, ii);
	guint jj;
	gboolean edge_exists = FALSE;
	for ( jj = 0 ; !edge_exists && jj < cn->in_edges->len ; jj++ ) {
	  GraphEdge *pm = g_ptr_array_index (cn->in_edges, jj);
	  if ( pm == ce ) {
	    edge_exists = TRUE;
	    cn = pm->from_node;
	  }
	}
	g_assert (edge_exists);
      }
      g_assert (cn == start_node);
    }
    else {
      g_assert (edges->length == 0);
    }
    */
    *path = edges;
  }

  else {
    g_queue_free (edges);
  }
  
  return found;
}

gboolean
graph_node_in_cycle (Graph *self, GraphNode *node, GQueue **path)
{
  if ( path != NULL) {
    *path = NULL;
  }
  
  // If there are no out_edges, we still need to fill in the path
  // queue as promised, if the user asked for it.
  if ( path != NULL && node->out_edges->len == 0 ) {
    *path = g_queue_new ();
  }
  else {
    // Starting with each neighbor, search for node.
    guint ii;
    for ( ii = 0 ; ii < node->out_edges->len ; ii++ ) {
      GraphEdge *edge = g_ptr_array_index (node->out_edges, ii);
      GraphNode *neighbor = edge->to_node;
      // Remove any path GQueue left over from the last neighbor.
      if ( *path != NULL ) {
	g_queue_free (*path);
      }
      gboolean reachable
	= graph_depth_first_search_for_node (self, neighbor, node, path);
      if ( reachable ) {
	if ( path != NULL ) {
	  g_queue_push_tail (*path, edge);
	}
	return TRUE;
      }
    }
  }

  return FALSE;
}

void
graph_dump_by_edges (Graph *self, const char *dot_file, GHashTable *names)
{
  // Open output file.
  FILE *of = fopen (dot_file, "w");
  g_assert (of != NULL);

  fprintf (of, "digraph \"%p\" {\n", self);

  guint ii;
  for ( ii  = 0 ; ii < self->edges->len ; ii++ ) {
    GraphEdge *ce = g_ptr_array_index (self->edges, ii);
    GraphNode *fn = ce->from_node, *tn = ce->to_node;

    GString *fnn = g_string_new (""), *tnn = g_string_new ("");

    if ( names != NULL ) {
      GString *lfnn = g_hash_table_lookup (names, fn);
      if ( lfnn != NULL ) {
	g_string_append (fnn, lfnn->str);
      }
      else {
	g_string_append_printf (fnn, "%p", fn);
      }
      
      GString *ltnn = g_hash_table_lookup (names, tn);
      if ( ltnn != NULL ) {
	g_string_append (tnn, ltnn->str);
      }
      else {
	g_string_append_printf (tnn, "%p", tn);
      }
    }
    else {
      g_string_append_printf (fnn, "%p", fn);
      g_string_append_printf (tnn, "%p", tn);
    }
    
    fprintf (of, "\"%s\" -> \"%s\"\n", fnn->str, tnn->str);
  }

  fprintf (of, "}\n");

  int return_code = fclose (of);
  g_assert (return_code == 0);
}

Graph *
graph_ref (Graph *self)
{
  g_assert (self->reference_count >= 1);
  self->reference_count++;
  
  return self;
}

// Decrement reference count, invoking graph_free if reference count
// drops to zero.
void
graph_unref (Graph *self)
{
  g_assert (self->reference_count >= 1);

  self->reference_count--;
  if ( self->reference_count == 0 ) {
    graph_free (self);
  }
}

// Free self, regardless of reference count.
void
graph_free (Graph *self)
{
  // Remove all the edges.
  gint ii;
  for ( ii = 0 ; ii < self->edges->len ; ii++ ) {
    graph_remove_edge (self, g_ptr_array_index (self->edges, ii));
  }

  // Remove all the nodes.
  for ( ii = 0 ; ii < self->nodes->len ; ii++ ) {
    graph_remove_node (self, g_ptr_array_index (self->nodes, ii));
  }

  g_free (self);
}
