// Test code for the cycle detection functionality of the Graph
// class.

#include <stdlib.h>

#include <glib.h>

#include "graph.h"

int
main (void)
{
  Graph *test_graph = graph_new ();

  GraphNode *a = graph_add_node (test_graph, NULL);
  GraphNode *b = graph_add_node (test_graph, NULL);
  GraphNode *c = graph_add_node (test_graph, NULL);
  GraphNode *c1 = graph_add_node (test_graph, NULL);
  GraphNode *c2 = graph_add_node (test_graph, NULL);
  GraphNode *c11 = graph_add_node (test_graph, NULL);
  GraphNode *d = graph_add_node (test_graph, NULL);

  GraphEdge *ab = graph_add_edge (test_graph, a, b, NULL);
  GraphEdge *bc = graph_add_edge (test_graph, b, c, NULL);
  GraphEdge *cd = graph_add_edge (test_graph, c, d, NULL);
  GraphEdge *da = graph_add_edge (test_graph, d, a, NULL);

  graph_add_edge (test_graph, c, c1, NULL);
  graph_add_edge (test_graph, c1, c11, NULL);
  graph_add_edge (test_graph, c, c2, NULL);

  graph_dump_by_edges (test_graph, "test_graph.dot", NULL);

  GQueue *path;
  gboolean result = graph_node_in_cycle (test_graph, a, &path);
  g_assert (result);
  g_assert (g_queue_peek_head (path) == da);
  g_assert (g_queue_peek_nth (path, 1) == cd);
  g_assert (g_queue_peek_nth (path, 2) == bc);
  g_assert (g_queue_peek_tail (path) == ab);

  g_queue_free (path);

  exit (EXIT_SUCCESS);
}
