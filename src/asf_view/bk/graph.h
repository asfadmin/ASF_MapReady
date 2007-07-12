// A directed graph.  Multiple edges going in the same direction
// between a pair of nodes are permitted (I forget the graph theory
// jargon for this).

#include <glib.h>

typedef struct {
  GPtrArray *in_edges;		// Incoming incident GraphEdge instances.
  GPtrArray *out_edges;		// Outgoing incident GraphEdge instances.
  gpointer contents;		// Node contents.
} GraphNode;

typedef struct {
  GraphNode *from_node;
  GraphNode *to_node;
  gpointer contents;		// Edge contents.
} GraphEdge;

typedef struct {
  GPtrArray *nodes;		// Array of GraphNode instances.
  GPtrArray *edges;		// Array of GraphEdge instances.
  gint reference_count;
} Graph;

// Create a new empty graph.
Graph *
graph_new (void);

// Add a new disconnected node with contents contents to self.
GraphNode *
graph_add_node (Graph *self, gpointer contents);

// Add a new edge with contents from from_node to to_node.
GraphEdge *
graph_add_edge (Graph *self, GraphNode *from_node, GraphNode *to_node,
		gpointer contents);

// Remove edge from self.  The caller is responsible for dealing with
// the edge contents before calling this method, but edge itself is
// always freed.
void
graph_remove_edge (Graph *self, GraphEdge *edge);

// Try to remove an edge from from_node to to_node.  There is no
// gaurantee about which edge is removed if more than one exists.  If
// an edge is found and removed, TRUE is returned, if no edge is found
// FALSE is returned.  FIIXME: this doesn't handle contents, should
// probably take a function that can dispose of the contents as
// needed.
gboolean
graph_try_remove_edge (Graph *self, GraphNode *from_node, GraphNode *to_node);

// Remove node from self.  Node must be completely disconnected.  The
// caller is responsible for dealing with the node contents before
// calling this method, but node itself is always freed.
void
graph_remove_node (Graph *self, GraphNode *node);

// Detmine if subject is reachable from start_node using a depth-first
// search.  If the subject is non-NULL, return in it a new GQueue
// (owned by the caller) of GraphEdge pointers (owned by self) that
// make up the path found, or a new empty GQueue if no path is found
// of if start_node and subject are identical.  The head of path is
// the edge into subject, and the tail is the edge from start_node.
gboolean
graph_depth_first_search_for_node (Graph *self, GraphNode *start_node,
				   GraphNode *subject, GQueue **path);

// Return true iff node is in at least one cycle.  If the node is in a
// cycle and path is non-NULL, return in it a new GQueue (owned by the
// caller) of GraphEdge pointers (owned by self) that make up the
// cycle found, or a new empty GQueue if no cycle is found.  The head
// of path is the edge into self, and the tail is the edge out of
// self.
gboolean
graph_node_in_cycle (Graph *self, GraphNode *node, GQueue **path);

// Dump self to dot_file by adding a dot entry for each edge.  Note
// that this approach means that nodes without edges are not dumped.
// The edges are dumped in the language understood by the graphviz
// program.  If names is non-NULL, an attempt is made for each node to
// look up a GString instances in names for the address of node, and
// the resulting name is used for the node in the graph output if it
// is found.  For nodes for which no name is found (or all nodes if
// names is NULL), the nodes labels are simply the pointer values of
// the individual nodes.  The graph name is the address self.
void
graph_dump_by_edges (Graph *self, const char *dot_file, GHashTable *names);

// Increment reference count.
Graph *
graph_ref (Graph *self);

// Decrement reference count, invoking graph_free if reference count
// drops to zero.  Note that it is up to the client to ensure that the
// contents of the nodes are already free.
void
graph_unref (Graph *self);

// Free self, regardless of reference count.  It is up to the client
// to ensure that the contents of the nodes are already free.
void
graph_free (Graph *self);
